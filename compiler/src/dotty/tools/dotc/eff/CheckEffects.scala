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

  def onlyEffCheckKill(using Context): Boolean = true // make this dependent on some compiler flag or smth?

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
        case cr => // ThisType or one of the special capabilities
          println(s"$cr <- is being turned into tree!")
          // untpd.Ident(new Name(cr.toString)).withType(cr)
          EmptyTree
end CheckEffects

/**
 * TODO:
 * 1. In setup - have a symtransformer that transforms every kill effect into a
 *  Kill Annotation, which will either take in a capture set or a Refs. Make sure that this
 * kill annotation does not have any special capabilities in it (special as defined in CaptureRef).
 * 2. For sym denotations from capture checker, have methods which call cc methods, but atPhase(CCPhase)
 *
 * FUNCTION SELF REF NOTE
 *
 * if function takes in function that kills itself, and calls it, function should say that the function is
 * killed inside the function
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
      atPhase(checkCapturesPhase)(getFP)

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

    override def skipRecheck(sym: Symbol)(using Context): Boolean =
      completed.contains(sym)

    extension [T <: Tree](tree: T)
      private def hasCCType: Boolean = cc.hasNuType(tree)
      private def ccType(using Context): Type = cc.nuType(tree)

    private def captures(tree: Tree)(using Context): Refs =
      atPhase(checkCapturesPhase)(tree.ccType.deepCaptureSet.elems)

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
      val used = if sym.is(Method) then tree.markedFree ++ sym.captureVars
        else tree.markedFree
      if !used.elems.isEmpty then
        val usedFootprint = used.elems.footprint
        for ref <- usedFootprint do
          val stripped = ref.stripReach.stripReadOnly.stripMaybe
          if killed.contains(stripped) then
            report.error(i"Use of ${tree} is forbidden.\nIt captures ${ref} which is killed.", tree.srcPos)
      super.recheckIdent(tree, pt)

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
      val paramRefs = tree.termParamss.flatten.flatMap(_.toCapabilities)
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
              // if (tree.name.toString == "kFile") then
              //   println(tree.tpe.widen)
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
      val formalDead = resType match
        case KillType(_, refs) =>
          // Can filter out function self ref probably
          refs.filterConserve(!_.symbol.isFuncSelfRef).flatMap(_.toCapabilities)
        case _ => Nil

      for param <- paramRefs do
        if rhsKilled.contains(param) then
          if formalDead.isEmpty then
            report.error(i"Parameter ${param} is killed in ${sym} but ${sym} has no kill annotation!",
            tree.srcPos)
          else if !formalDead.contains(param) then
            report.error(i"Kill set of ${sym} does not contain killed parameter ${param}",
            tree.srcPos)

      for ref <- capturedRefs.elems do
        if (rhsKilled.contains(ref)) then
          if formalDead.isEmpty then
            report.error(i"Captured variable ${ref} is killed in ${sym} but ${sym} has no kill annotation!",
            tree.srcPos)
          else if !formalDead.contains(ref) then
            report.error(i"Kill set of ${sym} does not contain killed capture variable ${ref}",
            tree.srcPos)

      val rhsCaptures = captures(tree.rhs).footprint
      for ref <- rhsCaptures do
        if formalDead.contains(ref) then
          report.error(i"Killed ${ref} cannot be captured in method result expression.", tree.srcPos)
      resType
    end checkExplicitDefDef

    // protected override def recheckArg(arg: Tree, formal: Type, pref: ParamRef, app: Apply)(using Context): Type =
    //   println("-------------------")
    //   println(arg.tpe)
    //   println(arg.ccType)
    //   recheck(arg, formal)
    /**
     *
     * Currently recheckApply allows killing capabilities with empty capture sets -
     * should this be forbidden - I think so.
     *
     * Then we allow only passing into kill function 1. capabilities without empty capture sets
     * 2. values
     *
     * TODO - forbid killing non-capabilites (trackable capture refs that are not actually tracked)
     *
     * Current behavior
     * 1. Killing a value does nothing
     * 2. Killing the top capability cap does nothing
     * 3. Killing a free variable means killing self
     */
    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val appType = super.recheckApply(tree, pt)

      appType match
        case KillType(parent, refs) =>
          var killsSelf = false
          val deadRefs = SimpleIdentitySet(refs.filter { ref =>
             ref.tpe match
              case selfRef: Capability if selfRef.termSymbol.isFuncSelfRef =>
                killsSelf = true
                false
              case tp: Capability if tp.isTrackableRef && !tp.isTerminalCapability => true
              case _ => false
          }.flatMap(_.toCapabilities)*).footprint

          for ref <- deadRefs do
            // val currentOwner = ctx.owner // in future do role.dclSym like SepCheck
            // ref.pathRootOrShared match
            //   case ref: TermRef =>
            //     val refOwner = ref.symbol.maybeOwner.enclosingMethodOrClass
            //     if (currentOwner.enclosingMethodOrClass.isProperlyContainedIn(refOwner)) then
            //       report.error(i"Killing a non-local variable ${ref} is prohibited!", tree.srcPos)
            //   case _ =>
            killed += ref.stripReach.stripMaybe.stripReadOnly

          if killsSelf then
            val fn = tree.fun
            val (fntpe, fnCS) = fn match
              case Select(qual, _) => // func.apply() is Select(func, apply) for some closure func
                (qual.tpe, captures(qual).footprint)
              case _ =>
                (fn.tpe, fn.symbol.captureVars.elems)

            fntpe match
              case ref: Capability =>
                killed += ref
                killed.addAll(fnCS.iterator)
              case tp => println(tp)
        case _ =>
      appType.dropTopLevelKill
    end recheckApply

    /**
     * Note that the returned expression of a block can be a Typed() generated by
     * the compiler during the Typer phase, indicating that the expression is returned.
     */
    override def recheckTyped(tree: Typed)(using Context): Type =
      val tptType = recheck(tree.tpt)
      val forcedTpt = tree.tpt.asInstanceOf[TypeTree]
      forcedTpt match
        case _: InferredTypeTree =>
          recheck(tree.expr)
        case _ =>
          recheck(tree.expr, tptType)
    end recheckTyped

    private def recheckBlock(stats: List[Tree], expr: Tree)(using Context): Type =
      recheckStats(stats)
      val exprType = recheck(expr)
      // println(exprType)
      // println(exprType.widen.show)
      val avoided = avoidKill(exprType, localSyms(stats).filterConserve(_.isTerm))
      // println(avoided.show)
      // println("----------------------")
      avoided

    override def recheckBlock(tree: Block, pt: Type)(using Context): Type = tree match
      case Block(Nil, expr: Block) => recheckBlock(expr, pt)
      case Block((mdef: DefDef) :: Nil, closure: Closure) =>
        recheckClosureBlock(mdef, closure.withSpan(tree.span), pt)
      case Block(stats, expr) => recheckBlock(stats, expr)

    override def recheckClosureBlock(mdef: DefDef, expr: Closure, pt: Type)(using Context): Type =
        val sym = mdef.symbol
        sym.ensureCompleted() // unnecessary because calling sym.info below will complete it

        val newTpe = sym.info match
        case fntpe @ FunctionOrMethod(params, resType) =>
          if (fntpe.isKillFun) then
            recheckClosure(expr, pt, forceDependent = true)
          else
            recheckClosure(expr, pt, forceDependent = false)
        case tp =>
          // println(s"${tp} <- ${mdef.name.show}")
          recheckClosure(expr, pt, forceDependent = false)

        expr.setNuType(newTpe)
        newTpe
    end recheckClosureBlock

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
        case _ =>
          println(s"$body <- WHILE LOOP BODY")

      killed.addAll(loopKilled)
      defn.UnitType
    end recheckWhileDo

    override def recheckDef(tree: ValOrDefDef, sym: Symbol)(using Context): Type =
      try super.recheckDef(tree, sym)
      finally completed += sym

    /**
     *  Given some function type A ->{q1} B @kill(q2), we want to compute transitive closure
     *  of the function self refs by transitively replacing every function self ref with the function's qualifier (capture set).
     *
     *  first problem - function types may not always have an associated name/binding.
     *  Maybe solution is to partition functions with self refs into two categories - ones bound to name and ones that are not.
     *  The ones that are not bound to a name should not be added to saturated set, and this is okay since they aren't bound so
     *  can never be reached by something. Ones that are bound should be also be added.
     *
     *  second problem - in CT cap is used to for escaping rather than a function self-ref what to do there? There is problem
     *  where cap simultaneously represents fresh but also anything.
     *
     *  Tentative strategy:
     *  For each func: A->{q1} B @kill(q2), check if there exists any self ref in q2.
     *  If exists self ref, then we need to figure out how to get the capture set.
     *  .captureSet and .captureSetOfInfo methods only work correctly at the TermRef level
     *  or if the type is a RetainingType/CapturingType, and most of the time the expected type is
     *  not bound to any name (e.g. return type of function).
     *
     *  foo: A ->{q1} B @kill(q2) where FUN \in q2
     *  where bar \in q1
     *  and then
     *  bar: C ->{q3} D @kill(q4) where FUN \in q4
     *
     */
    private def saturate(tp: Type)(using Context): Type =
      val tm = new TypeMap:
        def apply(t: Type) = t match
          case tpe @ RetainingType(defn.FunctionTypeOfMethod(mt), refs) if mt.isKillFun =>
            val KillType(resType, killedRefs) = mt.resultType: @unchecked
            // println(killedRefs)
            val cs = atPhase(checkCapturesPhase)(tpe.captureSet)
            tpe
          case fntpe: MethodType if fntpe.isKillFun =>
            val KillType(resType, killedRefs) = fntpe.resultType: @unchecked
            fntpe
          case _ =>
            mapOver(t)
      tm(tp)

    /**
     * Given A ->{q1} B @kill(k1) <: A ->{q2} B @kill(k2), we need to
     * replace all function self refs in k1 and k2 with q1.
     *
     * The problem is that it is not easy to get capture set from a method type.
     *
     */
    override def checkConformsExpr(actual: Type, expected: Type, tree: Tree, addenda: Addenda)(using Context): Type =
      // val expected1 = saturate(expected)
      // println(atPhase(checkCapturesPhase)(expected.dropAllKill.captureSet))
      if !isCompatible(actual, expected) then
        // atPhase(checkCapturesPhase) {
        //   println("actual")
        //   println(actual)
        //   // actual match
        //   //   case RefinedType(parent, _, mt) =>
        //   //     println(parent)
        //   //     println(mt)
        //   //   case _ =>
        // }
        // println("---")

        err.typeMismatch(tree.withType(actual), expected, addenda)
      actual

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      unit.tpdTree = setup.setupUnit(unit.tpdTree, this)
      // denotPrinter().traverse(unit.tpdTree)
      super.checkUnit(unit)
      unit.tpdTree.removeAttachment(RecheckedTypes)

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

