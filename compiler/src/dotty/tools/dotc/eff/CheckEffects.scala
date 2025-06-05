package dotty.tools
package dotc
package eff

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*, Denotations.*
import ast.tpd, tpd.*
import transform.{PreRecheck, Recheck}
import annotation.tailrec
import cc.*
import Capabilities.*, CaptureSet.*
import Recheck.*
import NamerOps.{linkConstructorParams, methodType}
import util.SimpleIdentitySet
import Annotations.*
import config.Feature
import collection.mutable
import typer.ErrorReporting.{err, Addenda}

object CheckEffects:
  val name: String = "eff"
  val description: String = "effect checking"

  trait FXCheckerAPI:
    def recheckDef(tree: ValOrDefDef, sym: Symbol)(using Context): Type

    extension [T <: Tree](tree: T)
      def setCCType(tpe: Type): Unit
      def ccType(using Context): Type
  end FXCheckerAPI

  private var CheckEffectsPhase: Phase | Null = null // need to find better than this hack

  def isEffCheckingOrSetup(using Context): Boolean =
    val effId = CheckEffectsPhase match
      case null =>
        CheckEffectsPhase = ctx.base.allPhases.find(_.phaseName == "eff").getOrElse(NoPhase)
        CheckEffectsPhase.nn.id
      case phase =>
        phase.id
    val ctxId = ctx.phaseId
    ctxId == effId || ctxId == effId - 1

  // true if phase is effect checking, effect setup, capture checking, or capture setup
  def isEffOrCC(using Context): Boolean =
    isEffCheckingOrSetup || isCaptureCheckingOrSetup

  def onlyEffCheckKill(using Context): Boolean = true // make this dependent on some compiler flag or smth?

  def atCC[T](op: Context ?=> T)(using Context): T =
    atPhase(checkCapturesPhase)(op)

  private var killAnnot: ClassSymbol | Null = null // typestate.kill
  private var funcSelfRef: Symbol | Null = null // typestate.FUN
  private var effAnnot: ClassSymbol | Null = null // typestate.eff

  def getKillAnnot(using Context): ClassSymbol =
    killAnnot match
      case null =>
        killAnnot = requiredClass("typestate.kill")
        killAnnot.nn
      case annot => annot

  // Note that function self reference is a CaptureRef, but not a TrackableRef e.g. .isTrackableRef false
  def getFuncSelfRef(using Context): Symbol =
    funcSelfRef match
      case null =>
        funcSelfRef = requiredModule("typestate.FUN")
        funcSelfRef.nn
      case sym => sym

  def makeFuncSelfRef(using Context): Tree = ref(getFuncSelfRef)

  def getEffAnnot(using Context): ClassSymbol =
    effAnnot match
      case null =>
        effAnnot = requiredClass("typestate.eff")
        effAnnot.nn
      case annot => annot

  extension (sym: Symbol)
    /**
     * -Yrecheck-test flag is used in compilation unit tests. There is a problem
     * where the tests will say a kill annotation is != to the killAnnot, and so
     * temporary solution is to add an additional compiler flag to the unit test compilation
     * which signals that one should check annotation symbol equality via names rather than whole symbol.
     *
     * This is less robust, but it should be fine as long as the compilation tests do not re-use the
     * name "kill" or "FUN" for a class.
     */
    def isKill(using Context): Boolean =
      if ctx.settings.YrecheckTest.value then
        sym.name == getKillAnnot.name
      else
        sym == getKillAnnot

    def isFuncSelfRef(using Context): Boolean =
      if ctx.settings.YrecheckTest.value then
        sym.name == getFuncSelfRef.name
      else
        sym == getFuncSelfRef

    def isEff(using Context): Boolean =
      effAnnot match
        case null =>
          effAnnot = requiredClass("typestate.eff")
          sym == effAnnot.nn
        case annot => sym == annot

  extension (cref: Capability)
    def refTree(using Context): Tree =
      import ast.untpd
      cref match
        case cr: TermRef => ref(cr)
        case cr: TermParamRef => untpd.Ident(cr.paramName).withType(cr)
        case cr: RootCapability => ref(defn.captureRoot)
        case cr => // only ThisType
          println(s"$cr <- is being turned into tree!")
          // untpd.Ident(new Name(cr.toString)).withType(cr)
          EmptyTree

    /**
     * Removes all derived capability annotations.
     */
    def stripAllDC(using Context): Capability =
        cref.stripReach.stripMaybe.stripReadOnly
end CheckEffects

/**
 * TODO:
 * 1. In setup - have a symtransformer that transforms every kill effect into a
 *  Kill Annotation, which will either take in a capture set or a Refs. Make sure that this
 * kill annotation does not have any special capabilities in it (special as defined in CaptureRef).
 * 2. For sym denotations from capture checker, have methods which call cc methods, but atPhase(CCPhase)
 *
 * TODO add .capturing to type of closure.
 * Also add CapturingType case to avoidance to do CT-style avoidance
  */
class CheckEffects extends Recheck:
  thisPhase =>

  import CheckEffects.*

  override def phaseName: String = CheckEffects.name

  override def description: String = CheckEffects.description

  // randomly Feature.ccEnabledSomewhere required for this even though previously it wasn't necessary?
  override def isRunnable(using Context) = super.isRunnable && Feature.ccEnabledSomewhere

  override def transformSym(symd: SymDenotation)(using Context): SymDenotation =
    val sym = symd.symbol
    def updatedAfter(p: Phase): Boolean =
      sym.isUpdatedAfter(p) || p != preRecheckPhase && updatedAfter(p.next)
    if updatedAfter(checkCapturesPhase.prev)
    then atPhase(checkCapturesPhase.prev)(sym.denot.copySymDenotation())
    else symd

  def newRechecker()(using Context): Rechecker =
    val ccPhase = checkCapturesPhase.asInstanceOf[CheckCaptures]
    val captureChecker = ccPhase.checker match
      case null =>
        assert(false, s"Internal Error: Capture Checker not assigned at CC Phase.")
      case checker => checker
    if onlyEffCheckKill then
      KillChecker(ctx, captureChecker)
    else
      EffectChecker(ctx, captureChecker)
  end newRechecker

  extension (refs: Refs)
    private def footprint(using Context): Refs =
      atCC(getFP)

    private def getFP(using Context): Refs =
      def recur(elems: Refs, newElems: List[Capability]): Refs = newElems match
        case newElem :: newElems1 =>
          val superElems = newElem.captureSetOfInfo.elems.filter: superElem =>
            !superElem.isTerminalCapability && !elems.contains(superElem)
          recur(elems ++ superElems, newElems1 ++ superElems.toList)
        case Nil => elems
      val elems: Refs = refs.filter(!_.isTerminalCapability)
      recur(elems, elems.toList)
    end getFP

  /**
   *  TODO for supporting free variables
   * 1. more refined subtyping
   * 2. avoidance with function self ref in recheckBlock
   */
  class KillChecker(ictx: Context, cc: CheckCaptures.CheckerAPI) extends Rechecker(ictx), FXCheckerAPI:
    import CheckEffects.*
    import cc.*
    import KillOps.*

    // apparently this has to be in this class? (e.g. moving it out into CheckEffects breaks it).
    private val setup: FXSetupAPI = thisPhase.prev.asInstanceOf[FXSetup]

    private var killed: mutable.HashSet[Capability] = mutable.HashSet[Capability]()

    /*
     * 1. save all killed prior to op
     * 2. do op
     * 3. make new killed set of all refs killed while doing op
     * 4. reset killed to savedkilled
     * 5. return new killed
     *
     * This is really inefficient - find a better solution.
     * Probably the best way is to use something similar to ConsumedSet?
     */
    def segment(op: => Type): (Type, mutable.HashSet[Capability]) =
      val savedKilled = mutable.HashSet[Capability]()
      savedKilled.addAll(killed)

      val res = op

      val newKilled = killed.diff(savedKilled)
      killed = savedKilled
      (res, newKilled)

    private val keepNuTypes = false

    private val completed: mutable.HashSet[Symbol] = new mutable.HashSet[Symbol]

    // Maps etaExpanded closure type to TermRef of DefDef (dead code for now)
    private val etaExpanded: mutable.HashMap[Type, Type] = new mutable.HashMap[Type, Type]()

    override def skipRecheck(sym: Symbol)(using Context): Boolean =
      completed.contains(sym)

    extension [T <: Tree](tree: T)
      def setCCType(tpe: Type): Unit = cc.setNuType(tree)(tpe)
      def hasCCType: Boolean = cc.hasNuType(tree)
      def ccType(using Context): Type = cc.nuType(tree)

    private def captures(tree: Tree)(using Context): Refs =
      atCC(tree.ccType.deepCaptureSet.elems)

    private def boxedCaptures(tree: Tree)(using Context): Refs =
      atCC(tree.ccType.boxedCaptureSet.elems)

    override def recheckIdent(tree: Ident, pt: Type)(using Context): Type =
      val sym = tree.symbol

      tree.tpe match
        case ref: Capability if ref.isTrackableRef && !ref.isTerminalCapability =>
          if killed.contains(ref) then
            report.error(i"Use of killed variable ${tree} is forbidden.", tree.srcPos)
        case ref: Capability if !ref.isTerminalCapability =>
          if killed.contains(ref) then
            report.error(i"Use of self-killing function ${ref} is forbidden more than once!.", tree.srcPos)
        case _ =>

      // hack for tuples is to only get the captureVars if it is a Method
      // because tuple deconstruction results in sym.captureVars including things we don't want.
      val used = if sym.is(Method) then tree.markedFree ++ sym.captureVars ++ CaptureSet(captures(tree))
        else tree.markedFree ++ CaptureSet(captures(tree))
      if !used.elems.isEmpty then
        val usedFootprint = used.elems.footprint
        for ref <- usedFootprint do
          val stripped = ref.stripAllDC
          if killed.contains(stripped) then
            report.error(i"Use of ${tree} is forbidden.\nIt captures ${ref} which is killed.", tree.srcPos)
      super.recheckIdent(tree, pt)
    end recheckIdent

    override def recheckSelect(tree: Select, pt: Type)(using Context): Type =
      // tree.tpe match
      //   case t @ TermRef(inner @ TermRef(_, _), _) =>
      //     println(t.show)
      //     println(t.widen)
      //     println(killed)
      //   case _ =>

      recheckSelection(tree,
          recheck(tree.qualifier, selectionProto(tree, pt)).widenIfUnstable,
          tree.name, pt)
    end recheckSelect

    /**
     * Problem: Match expressions get lowered into Labeled trees (c.f. patternMatcher phase) of form similar to:
     *
     matchResult1[resType]: {
      case val scrutinee = ...
      if scrutinee == branch1 then
        return [matchResult1] {
          body of branch1
        }
      else ()
      if scrutinee == branch2 then
        return[matchResult1] {
          body of branch2
        }
      else ()
      ...
      }
     * Note that this there can still be match inside the expr of the Labeled if the match only is on simple switches
     * where every branch is integer or string constant.
     *
     * Strategy is to add the killed set to the type of the Return expr if it is returning to a labeled, which can
     * be checked by using return.from.sym.is(Method) probably.
     */
    override def recheckLabeled(tree: Labeled, pt: Type)(using Context): Type = tree match
      case Labeled(bind, expr) =>
        val (bindType: NamedType) = recheck(bind, pt): @unchecked
        val exprType = recheck(expr, defn.UnitType)
        val block = expr.asInstanceOf[Block]
        bindType.dropTopLevelKill // temporary salve

    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Type =
      val resTree = tree.tpt
      val resType = recheck(tree.tpt)
      def isUninitWildcard = tree.rhs match
        case Ident(nme.WILDCARD) => tree.symbol.is(Mutable)
        case _ => false
      if tree.rhs.isEmpty || isUninitWildcard || !sym.exists || sym.is(Module) then resType
      else
        resTree match
          case _: InferredTypeTree =>
            recheck(tree.rhs) // we infer!
          case _ =>
            recheck(tree.rhs, resType)
    end recheckValDef

    /**
     * Algorithm for explicitly given result type (restype)
     * 1. recheck rhs against restype with top level kill dropped, getting everything killed in rhs (rhsKilled)
     * 2. check if explicitly given result type accounts for all killed parameters
     * 3. check if explicitly given result type accounts for all killed captured variables (maybe change to free?)
     * 4. importantly, do not add rhsKilled to the global killed set - this is okay due to following reasoning:
     *  1. Parameters - these only exist within the defdef, so irrelevant outside
     *  2. Local variables - these only exist within defdef, so irrelevant outside
     *  3. Free variables - we don't want these to be killed yet, we only want them to be killed when defdef is applied, so
     *     we don't want them in the killed set.
     *
     * For inferDefDef we do same but instead of checking we add the killed stuff.
     */
    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Type =
      val resTree = tree.tpt
      val paramRefs = tree.termParamss.flatten.flatMap(toCapabilities)
      val capturedRefs = sym.captureVars

      inContext(linkConstructorParams(sym).withOwner(sym)):
        val resType = recheck(resTree) // totally unnecessary
        if tree.rhs.isEmpty || sym.isInlineMethod || sym.isEffectivelyErased
        then resType
        else
          resTree match
            case _: InferredTypeTree =>
              val (rhsType, rhsKilled) = segment(recheck(tree.rhs)) // we infer
              inferDefDef(tree, sym, rhsType, paramRefs, rhsKilled, capturedRefs)
            case _ =>
              val (_, rhsKilled) = segment(recheck(tree.rhs, resType.dropTopLevelKill)) // we discard rhsType
              checkExplicitDefDef(tree, sym, resType, paramRefs, rhsKilled, capturedRefs)
    end recheckDefDef

    /**
     * Plan - do inference on the tree.tpt and return new inferred, but then
     * in FXSetup when constructing the new MethodType integrate the params
     * into the KillType()
     *
     * Note that we do not need to check for what is dead may not return here
     * since that would be naturally checked by recheckIdent as killed variables can only
     * occur via application if the DefDef is inferred
     */
    def inferDefDef(tree: DefDef, sym: Symbol, rhsType: Type, paramRefs: List[Capability],
      rhsKilled: mutable.HashSet[Capability], capturedRefs: CaptureSet)(using Context): Type =
      val allKilled =
        (paramRefs
          .filter(rhsKilled.contains(_))
          .map(_.refTree))
        :::
        (capturedRefs.elems
          .filter(rhsKilled.contains(_))
          .map(_.refTree).toList)

      if !allKilled.isEmpty then
        KillType(rhsType, allKilled)
      else rhsType
    end inferDefDef

    /**
     * Checks that explicitly given type accounts for all killed parameters,
     * and that the anything capturing a killed parameter is not returned.
     *
     * Hopefully it shouldn't be possible for a something capturing a parameter to be
     * killed and the parameter is somehow not in the killed set.
     */
    def checkExplicitDefDef(tree: DefDef, sym: Symbol, resType: Type, paramRefs: List[Capability],
      rhsKilled: mutable.HashSet[Capability], capturedRefs: CaptureSet)(using Context): Type =
      val (killsSelf, formalDead) = resType match
        case KillType(_, refs) =>
          val (killsSelf, cleanedRefs) = cleanSelfRef(refs)
          (killsSelf, cleanedRefs.flatMap(toCapabilities))
        case _ => (false, Nil)

      for param <- paramRefs do
        if rhsKilled.contains(param) then
          if formalDead.isEmpty && !killsSelf then
            report.error(i"Parameter ${param} is killed in ${sym} but ${sym} has no kill annotation!",
            tree.srcPos)
          else if !formalDead.contains(param) then
            report.error(i"Kill set of ${sym} does not contain killed parameter ${param}",
            tree.srcPos)

      if !killsSelf then
        for ref <- capturedRefs.elems do
          if rhsKilled.contains(ref) then
            if formalDead.isEmpty then
              report.error(i"Captured variable ${ref} is killed in ${sym} but ${sym} has no kill annotation!",
              tree.srcPos)
            else if !formalDead.contains(ref) then
              report.error(i"Kill set of ${sym} does not contain killed capture variable ${ref}",
              tree.srcPos)
      end if

      val rhsCaptures = captures(tree.rhs).footprint
      for ref <- rhsCaptures do
        if formalDead.contains(ref) then
          report.error(i"Killed ${ref} cannot be captured in method result expression.", tree.srcPos)
      resType
    end checkExplicitDefDef

    protected override def recheckArg(arg: Tree, formal: Type, pref: ParamRef, app: Apply)(using Context): Type =
      // println("-------------------")
      // // println(arg)
      // // println(arg.tpe)
      // // println(atCC(arg.ccType.widen))
      // arg match
      //   case Block(_, Block((mdef: DefDef) :: Nil, closure: Closure)) =>
      //     println(arg.ccType)
      //   case _ =>
      // println(s"${atCC(arg.ccType.widen)} <- ccType")
      recheck(arg, formal)

    /**
     * TODO - deal with param dependent functions -> something like
     * def foo(f: File^, y: () => Unit @kill(f)) = ...
     *
     * Note that it is possible for a tree to kill a value that is not a capability. In particular,
     * function values that are killed must have their capture set killed.
     *
     * Rules for kill:
     * 1. Killing a non function "value" should do nothing - so first level TypeRefs, ConstantTypes, e.g. new File, 5, etc.
     * 2. If we kill a ObjectCapability with an EMPTY capture set, then it it ILLEGAL!
     * 3. Killing a terminal capability does not do anything (can also be error i guess)
     * 4. If something is killed that is not a Capability but has a capture set, treat it as a capability because it is
     * probably a application to an anonymous function (or eta expansion), which means we will kill its qualifier
     *
     * TODO - deal with polymorphic functions. In particular, we want to make sure that the type variable instantiation is good.
     * For example, def f[T <: Int](x: T): Unit @kill(x). When we apply f() to something, we would like to give an error.
     * Or if its not bounded but we instantiate T with a reference to not a capability (this will probably be done in TypeApply).
     */
    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val fn = tree.fun
      val args = tree.args
      val (funtpe0, qualType) = fn match
        case fun: Select =>
          val qualType = recheck(fun.qualifier, selectionProto(fun, WildcardType)).widenIfUnstable
          (recheckSelection(fun, qualType, fun.name, WildcardType), qualType)
        case _ =>
          (recheck(tree.fun), NoType)
      val funtpe1 = if tree.fun.symbol.originalSignaturePolymorphic.exists then tree.fun.tpe else funtpe0

      val fntpe = funtpe1.widen match
        case fntpe: MethodType => fntpe
        case tp =>
          assert(false, i"unexpected type of ${tree.fun} at recheckApply: $tp")

      assert(fntpe.paramInfos.hasSameLengthAs(tree.args))
      val formals = fntpe.paramInfos

      def recheckArgs(args: List[Tree], formals: List[Type], prefs: List[ParamRef]): List[Type] = args match
        case arg :: args1 =>
          val argType = recheckArg(arg, normalizeByName(formals.head), prefs.head, tree)
          val formals1 =
            if fntpe.isParamDependent
            then formals.tail.map(_.substParam(prefs.head, argType))
            else formals.tail
          argType :: recheckArgs(args1, formals1, prefs.tail)
        case Nil =>
          assert(formals.isEmpty)
          Nil
      end recheckArgs
      val argTypes = recheckArgs(args, formals, fntpe.paramRefs)

      var fntpeIsKill = false
      /**
       * Q: Why is map from parameter name to arg tree okay?
       * 1. Duplicate parameter names prohibited
       * 2. Parameters bind the tighest, so a another variable outside with the same name as a parameter will not bind.
       *
       * Q: Why do other maps fail?
       * 1. Tree to Tree - parameter substitution results in tree equality failing (probably due to type of tree
       * tree changes.)
       * 2. Symbol to Tree - TermParamRefs do not have symbols.
       */
      val deadRefNameToArg = new util.EqHashMap[Name, Tree]()
      fntpe.resType match
        case KillType(_, refs) =>
          fntpeIsKill = true
          for ref <- refs do
            ref.tpe match
              case tp: TermParamRef if tp.binder == fntpe =>
                assert(ref.isInstanceOf[Ident], i"TermParamRef ${ref} is not an Ident at ${tree}.")
                val toIdent = ref.asInstanceOf[Ident]
                assert(toIdent.name == tp.paramName,
                  i"TermParamRef ${ref} does not have the name ${tp.paramName}, as its Ident name ${toIdent.name} at ${tree}.")
                deadRefNameToArg.update(tp.paramName, args(tp.paramNum))
              case _ =>
        case _ =>

      val appType = recheckApplication(tree, qualType, fntpe, argTypes)

      appType match
        case KillType(parent, refs) =>
          assert(fntpeIsKill, i"${tree.fun} is not kill function but application type ${appType} is a top level kill type!")
          var killsSelf = false
          // first pass filters out function self ref + all non-function non-termref value types.
          // these are all the stuff that will definitely NOT be added to the killed set.
          val refs1 = refs.filterConserve: ref =>
            ref.tpe match
              case _: ConstantType => false
              case _: TypeRef => false
              case tp: CoreCapability if tp.isTerminalCapability => false
              case selfRef: CoreCapability if selfRef.termSymbol.isFuncSelfRef =>
                killsSelf = true
                false
              case _ => true

          for ref <- refs1 do
            ref.tpe match
              case tp: CoreCapability =>
                if !tp.isTrackableRef then
                  report.error(em"Illegal capture ref ${ref} is being killed!", tree.srcPos)
                else if atCC(captures(ref).isEmpty) && !tp.derivesFrom(defn.Caps_Capability) then
                  tp match
                    case ref: TermRef if ref.typeSymbol.isTypeParam =>
                    case _ =>
                      report.error(em"Capture set of ${ref} is empty! It cannot be killed!", tree.srcPos)
              case _ =>
                // if atCC(captures(ref).isEmpty) then
                //   report.error(em"Not a capability wrapper!", tree.srcPos)
          end for

          val ka = new mutable.ListBuffer[Tree]()
          val kf = new mutable.ListBuffer[Tree]()

          // deadRefNameToArg maps killed ref symbols to tree of arg if ref is killed arg
          for case ref @ Ident(name) <- refs1 do
            deadRefNameToArg.lookup(name) match
              case null => kf += ref // we add the dead ref tree directly to kf
              case tree => ka += tree // we add the argument tree to the ka

          val killedArgs = ka.toList
          val killedFree = kf.toList

          for arg <- killedArgs do
            killed.addAll((boxedCaptures(arg) ++ captures(arg)).footprint.iterator.map(stripAllDC))

          val deadFree = SimpleIdentitySet(killedFree.filter { ref =>
            ref.tpe match
              case tp: ObjectCapability if tp.isTrackableRef => true
              case tp =>
                println(i"${tp} <- FILTER DEAD FREE AND FOUND")
                false
          }.flatMap(toCapabilities)*).footprint

          killed.addAll(deadFree.iterator.map(stripAllDC))

          val (fnRef, fnCS) = fn match
            case Select(qual, _) => // func.apply() is Select(func, apply) for some closure func
              (qual.tpe, captures(qual).footprint)
            case _ =>
              (fn.tpe, fn.symbol.captureVars.elems.footprint)

          fnRef match
            case cref: Capability =>
              if killsSelf then
                killed += cref
                killed.addAll(fnCS.iterator)
              else if !killedFree.isEmpty then
                killed += cref // if a function kills a free variable but not itself, then we only add the function to the kill set.
                  // this is because if we add the entire function qualifier, it is possible for the outer to kill something which
                  // is NOT in its captured variables. cf TODO
            case tp =>
              // println(i"${tp} <- fntpe match in recheckApply") // probably applying closure to something
        case _ =>
          assert(!fntpeIsKill, i"${tree.fun} is kill function but application type ${appType} is not top level kill type!")
      appType.dropTopLevelKill
    end recheckApply

    /**
     * Note that the returned expression of a block can be a Typed generated by
     * the compiler during the Typer phase, indicating that the expression is returned.
     */
    override def recheckTyped(tree: Typed)(using Context): Type =
      val tptType = recheck(tree.tpt)
      val forcedTpt = tree.tpt.asInstanceOf[TypeTree]
      if forcedTpt.isInferred then
        recheck(tree.expr)
      else
        recheck(tree.expr, tptType)
        tptType
    end recheckTyped

    private def recheckBlock(stats: List[Tree], expr: Tree)(using Context): Type =
      recheckStats(stats)
      val exprType = recheck(expr)
      // println(exprType.show)
      // println("========")
      val avoided = avoidKill(exprType, localSyms(stats).filterConserve(_.isTerm))
      // println("========")
      // println(avoided.show)
      // println("----------------------")
      avoided

    override def recheckBlock(tree: Block, pt: Type)(using Context): Type = tree match
      case Block(Nil, expr: Block) => recheckBlock(expr, pt)
      case Block((mdef: DefDef) :: Nil, closure: Closure) =>
        recheckClosureBlock(mdef, closure.withSpan(tree.span), pt)
      case Block(stats, expr) => recheckBlock(stats, expr)
    end recheckBlock

    /**
     * I would like to add the captured vars of the defdef
     * to the newTpe, but the problem is with avoidance.
     *
     * If we try to use RetainingType, the avoidance algorithm will not properly work on them.
     * So then we should be using CapturingType with CaptureAnnotations, but then
     * the major problem is that to do avoidance we must be at the capture checking phase since they are only valid at CC,
     * but then, if we do use atCC(avoidKill(...)) to do avoidance, what will happen is that TermRefs will get widened
     * to their type at the capture checking phase, which is quite bad since this loses all kill information.
     */
    override def recheckClosureBlock(mdef: DefDef, expr: Closure, pt: Type)(using Context): Type =
        val sym = mdef.symbol
        if !sym.isCompleted then
          sym.ensureCompleted()
        else
          recheckDef(mdef, sym)

        val newTpe = recheckClosure(expr, pt, forceDependent = true)

        expr.setNuType(newTpe)
        newTpe
    end recheckClosureBlock

    /**
     * TODO: do lub for two kill functions
     *
     * lub is done by via an OrType.
     * For CT, they have one special case in TypeComparer.distributeOr which essentially
     * just combines the top level refs and then recursively lubs the parents.
     * This works for CT because its always at the top level wrapping any
     * function.
     */
    override def recheckIf(tree: If, pt: Type)(using Context): Type =
      recheck(tree.cond, defn.BooleanType)

      val savedKilled = mutable.HashSet[Capability]()
      savedKilled.addAll(killed)

      val tBranch = recheck(tree.thenp, pt)
      val tKilled = killed.diff(savedKilled)
      killed = savedKilled

      val eBranch = recheck(tree.elsep, pt)
      killed.addAll(tKilled)
      tBranch | eBranch
    end recheckIf

    override def recheckMatch(tree: Match, pt: Type)(using Context): Type =
      val selectorType = recheck(tree.selector, pt)
      val typesAndKill =
        for cas <- tree.cases yield
          segment(recheckCase(cas, selectorType.widen, pt))

      typesAndKill.foreach(killed ++= _._2.iterator)
      val casesType = typesAndKill.map(_._1)
      TypeComparer.lub(casesType)

    /**
     * TODO: do Labeled inference via adding types - see recheckLabeled comment
     */
    override def recheckReturn(tree: Return)(using Context): Type =
      def avoidMap = new TypeOps.AvoidMap:
        def toAvoid(tp: NamedType) =
           tp.symbol.is(Case) && tp.symbol.owner.isContainedIn(ctx.owner)

      val rawType = recheck(tree.expr)
      val ownType = avoidMap(rawType)
      def widened(tp: Type): Type = tp match
        case tp: SingletonType => tp.widen
        case tp: AndOrType => tp.derivedAndOrType(widened(tp.tp1), widened(tp.tp2))
        case tp @ AnnotatedType(tp1, ann) => tp.derivedAnnotatedType(widened(tp1), ann)
        case _ => tp
      checkConforms(ownType, widened(tree.from.symbol.returnProto), tree)
      defn.NothingType
    end recheckReturn

    // Note that preventing killing free variables should already prevent self-killing functions.
    // I cannot find function to compute free variables of tree wrt to enclosing block so use localSyms
    override def recheckWhileDo(tree: WhileDo)(using Context): Type =
      recheck(tree.cond, defn.BooleanType)
      val body = tree.body
      val (_, loopKilled) = segment(recheck(body, defn.UnitType))
      body match
        case Block(stats, expr) =>
          val bound = localSyms(stats)
          if !(loopKilled.map(_.asInstanceOf[ObjectCapability].termSymbol).subtractAll(bound).isEmpty) then
            report.error("Killing a free variable is prohibited in loop body!", body.srcPos)
        case _ => // should only be loop with one expression - in which case it has no local variables and so killing is not okay.
          if !(loopKilled.isEmpty) then
            report.error("Killing a free variable is prohibited in loop body!", body.srcPos)
          // println(s"$body <- WHILE LOOP BODY")

      killed.addAll(loopKilled) // unnecessary
      defn.UnitType
    end recheckWhileDo

    override def recheckDef(tree: ValOrDefDef, sym: Symbol)(using Context): Type =
      try super.recheckDef(tree, sym)
      finally completed += sym

    /**
     *  Given A ->{q1} B @kill(k1) <: A ->{q2} B @kill(k2), we need to
     *  replace all function self refs in k1 and k2 with q1
     *
     *  Stupid Hack:
     *  An inferred function type will only have FUN in kill set via avoidance. Then say
     *  the function's kill set is some q, FUN. Therefore, the function's qualifier must be
     *  {q, cap}, as there must be avoidance performed in the qualifier as well, and the qualifier must be inferred.
     *
     *  An explicit function type can have FUN in kill set without avoidance, e.g. if user
     *  explicitly gives type as A ->{l, k} B @kill(FUN). But then the function type should be
     *  wrapped in a RetainingType(..., {l, k}) and we can extract the funcion qualifier from that.
     *
     *  It is possible for there to be a mix of the two - for example
     *  val foo = () =>
          val bar: A ->{...} B = ...
          ...
          bar
     * But the logic should still work.
     *
     * Idea #2:
     *  Problem - it is difficult to get the type of some tree at CC phase.
     *  Major reason - because CC phase only saves types which go through recheckFinish
     *  For example, in infer example only the top level Block(_, Block(...)) has its type
     *  saved in nuTypes, since only that reaches recheckFinish as it is the direct argument
     *  of the function.
     *
     *  It is probably okay to add tree.setNuType to more functions - this is because
     *  even though the type of tree may change after it is rechecked due to subtyping (generally CC being constraint solver)
     *  the actual reference to the type should remain the same, and so the nuType should be changed.
     *  But then what do we do with this nuType?
     *  The next problem is that methods inherently lack a capture set. Only when they become closures do they get the
     *  CapturingType wrapping the MethodType. Then consider how we recheck closure blocks
     *  infer defdef type -> infer closure type with defdef type -> give closure type to block.
     *  But then when we infer closure type with defdef type, we toss away the old closure type. But now we have to consider
     *  it even when inferring. Probably we have to map the capture set from the ccType to the type of the closure in recheckClosureBlock.
     */
    private def saturate(tp: Type)(using Context): Type =
      val tpToCs: util.EqHashMap[Type, CaptureSet] = util.EqHashMap[Type, CaptureSet]()
      val mapCaptureSets = new TypeTraverser:
        def traverse(tp: Type): Unit = tp match
          case tp @ CapturingType(parent, _) =>
            if tpToCs.lookup(parent) == null then tpToCs(parent) = atCC(tp.captureSet)
            traverseChildren(tp)
          case _ => traverseChildren(tp)
      end mapCaptureSets

      mapCaptureSets.traverse(tp)

      val replaceSelfRef = new TypeMap:
        def apply(t: Type) = t match
          case tpe @ RefinedType(parent, nme.apply, mt: MethodType) if mt.isKillFun && defn.isFunctionNType(parent) =>
            val KillType(resType, killedRefs) = mt.resultType: @unchecked
            val (killsSelf, cleanedRefs) = cleanSelfRef(killedRefs)

            val newRefs = if killsSelf then
              tpToCs.lookup(tpe) match
                case null => cleanedRefs
                case cs =>
                  cleanedRefs ::: cs.elems.toList.map(refTree)
              else cleanedRefs

            tpe.derivedRefinedType(
              parent = mapOver(parent),
              refinedInfo =
                mt.derivedLambdaType(
                  paramInfos = mt.paramInfos.mapConserve(mapOver),
                  resType = KillType(mapOver(resType), newRefs)
                )
            )
          case tpe @ AppliedType(parent, targs) if defn.isFunctionNType(tpe) && targs.last.isKillType =>
            // an AppliedType will be a function of (targs.init) => targs.last
            val KillType(resType, killedRefs) = targs.last: @unchecked
            val (killsSelf, cleanedRefs) = cleanSelfRef(killedRefs)

            val newRefs = if killsSelf then
              tpToCs.lookup(tpe) match
                case null => cleanedRefs
                case cs =>
                  cleanedRefs ::: cs.elems.toList.map(refTree)
              else cleanedRefs

            tpe.derivedAppliedType(
              mapOver(parent),
              (targs.init.mapConserve(mapOver)) :+ KillType(mapOver(resType), newRefs)
            )
          case _ =>
            mapOver(t)
        end apply
      end replaceSelfRef
      replaceSelfRef(tp)
    end saturate

    override def checkConformsExpr(actual: Type, expected: Type, tree: Tree, addenda: Addenda)(using Context): Type =
      val actual1 = saturate(actual.widen)  // the widen hopefully shouldn't do anything bad.
      val expected1 = saturate(expected.widen)
      if !(actual eq expected) && !(isCompatible(actual1, expected1)) then
        err.typeMismatch(tree.withType(actual), expected, addenda)
      actual
    end checkConformsExpr

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      unit.tpdTree = setup.setupUnit(unit.tpdTree, this)
      // denotPrinter().traverse(unit.tpdTree)
      super.checkUnit(unit)
      unit.tpdTree.removeAttachment(RecheckedTypes)
    end checkUnit
  end KillChecker

  class EffectChecker(ictx: Context, cc: CheckCaptures.CheckerAPI) extends Rechecker(ictx), FXCheckerAPI:
    import CheckEffects.*
    import EffOps.*
    import cc.*

    private val setup: FXSetupAPI = thisPhase.prev.asInstanceOf[FXSetup]

    private var killed = util.HashSet[Capability]()

    // this should be like ConsumedSet - every defdef gets a fresh one
    // Is the only use of this is check what parameters a function uses
    private var used = util.HashSet[Capability]()

    private val completed = new collection.mutable.HashSet[Symbol]

    extension [T <: Tree](tree: T)
      def setCCType(tpe: Type): Unit = cc.setNuType(tree)(tpe)
      def hasCCType: Boolean = cc.hasNuType(tree)
      def ccType(using Context): Type = cc.nuType(tree)

    override def skipRecheck(sym: Symbol)(using Context): Boolean =
      completed.contains(sym)

    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Type =
      val resTree = tree.tpt
      val paramRefs = tree.termParamss.flatten.flatMap(_.toCapabilities)

      inContext(linkConstructorParams(sym).withOwner(sym)):
        val resType = recheck(resTree) // totally unnecessary
        if tree.rhs.isEmpty || sym.isInlineMethod || sym.isEffectivelyErased
        then resType
        else
          val rhsType = recheck(tree.rhs)
          resTree match
            case _: InferredTypeTree =>
              rhsType
            case _ =>
              checkExplicitDefDef(tree, sym, resType, paramRefs)
    end recheckDefDef

    def checkExplicitDefDef(tree: DefDef, sym: Symbol, resType: Type, params: List[Capability])(using Context): Type =
      val (formalUsed, formalDead) = resType match
        case EffectType(_, usedRefs, killedRefs) =>
          (usedRefs.flatMap(_.toCapabilities), killedRefs.flatMap(_.toCapabilities))
        case _ => (Nil, Nil)

      for param <- params do
        if killed.contains(param) then
          if !formalDead.contains(param) then
            report.error(i"Kill set of ${sym} does not contain killed parameter ${param}",
            tree.srcPos)
        if used.contains(param) then
          if !formalDead.contains(param) then
            report.error(i"Use set of ${sym} does not contain used parameter ${param}", tree.srcPos)
      resType
    end checkExplicitDefDef

    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val appType = super.recheckApply(tree, pt)
      appType match
        case EffectType(_, usedElems, killedElems) =>
          val usedRefs = SimpleIdentitySet(
            usedElems.filter { ref =>
            ref.tpe match
              case tp: Capability if tp.isTrackableRef && !tp.isTerminalCapability => true
              case _ => false
            }.flatMap(_.toCapabilities)*).footprint

          for ref <- usedRefs do
            if killed.contains(ref) then
              report.error(i"Use of killed variable ${ref} is forbidden!", tree.srcPos)
            used += ref.stripReach.stripMaybe.stripReadOnly

          val killedRefs = SimpleIdentitySet(
            killedElems.filter { ref =>
            ref.tpe match
              case tp: Capability if tp.isTrackableRef && !tp.isTerminalCapability => true
              case _ => false
            }.flatMap(_.toCapabilities)*).footprint

          for ref <- killedRefs do
            killed += ref.stripReach.stripMaybe.stripReadOnly
        case _ =>
      appType.dropTopLevelEff
    end recheckApply

    override def recheckDef(tree: ValOrDefDef, sym: Symbol)(using Context): Type =
      try super.recheckDef(tree, sym)
      finally completed += sym

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      unit.tpdTree = setup.setupUnit(unit.tpdTree, this)
      super.checkUnit(unit)

  end EffectChecker
end CheckEffects

