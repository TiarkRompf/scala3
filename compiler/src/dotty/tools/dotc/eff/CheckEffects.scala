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
import CaptureSet.*
import Recheck.*
import NamerOps.{linkConstructorParams, methodType}
import util.SimpleIdentitySet
import Annotations.*
import typer.ErrorReporting.Addenda
import config.Feature

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

  private var killAnnot: ClassSymbol | Null = null // very hacky solution for now

  def getKillAnnot(using Context): ClassSymbol =
    killAnnot match
      case null =>
        killAnnot = requiredClass("typestate.kill")
        killAnnot.nn
      case annot => annot

  extension (sym: Symbol)
    def isKill(using Context): Boolean =
      killAnnot match
        case null =>
          killAnnot = requiredClass("typestate.kill")
          sym == killAnnot.nn
        case annot => sym == annot

  private var effAnnot: ClassSymbol | Null = null

  def getEffAnnot(using Context): ClassSymbol =
    effAnnot match
      case null =>
        effAnnot = requiredClass("typestate.eff")
        effAnnot.nn
      case annot => annot

  extension (sym: Symbol)
    def isEff(using Context): Boolean =
      effAnnot match
        case null =>
          effAnnot = requiredClass("typestate.eff")
          sym == effAnnot.nn
        case annot => sym == annot
end CheckEffects

/**
 * TODO:
 * 1. In setup - have a symtransformer that transforms every kill effect into a
 *  Kill Annotation, which will either take in a capture set or a Refs. Make sure that this
 * kill annotation does not have any special capabilities in it (special as defined in CaptureRef).
 * 2. For sym denotations from capture checker, have methods which call cc methods, but atPhase(CCPhase)
 */
class CheckEffects extends Recheck:
  thisPhase =>

  import CheckEffects.*

  override def phaseName: String = CheckEffects.name

  override def description: String = CheckEffects.description

  // randomly Feature.ccEnabledSomewhere required for this even though previously it wasn't necessary?
  override def isRunnable(using Context) = super.isRunnable && Feature.ccEnabledSomewhere

  def newRechecker()(using Context): Rechecker =
    val ccPhase = checkCapturesPhase.asInstanceOf[CheckCaptures]
    val cc = ccPhase.checker match
      case null =>
        assert(false, s"Internal Error: Capture Checker not assigned at CC Phase.")
      case checker => checker
    if onlyEffCheckKill then
      KillChecker(ctx, cc)
    else
      EffectChecker(ctx, cc)


  class KillChecker(ictx: Context, cc: CheckCaptures.CheckerAPI) extends Rechecker(ictx), FXCheckerAPI:
    import CheckEffects.*
    import cc.*
    import KillOps.*

    // apparently this has to be in this class?
    private val setup: FXSetupAPI = thisPhase.prev.asInstanceOf[FXSetup]

    private var killed = util.HashSet[CaptureRef]()

    /*
     * 1. saved all killed prior to op
     * 2. do op
     * 3. reset killed to savedkilled
     * 4. return new killed
     *
     * This is really inefficient - find a better solution.
     * Probably the best way is to use something similar to ConsumedSet?
     */
    def segment(op: => Type): (Type, util.HashSet[CaptureRef]) =
      val savedKilled = util.HashSet[CaptureRef]()
      savedKilled ++= killed.iterator
      val res = op

      val newKilled = util.HashSet[CaptureRef]()
      newKilled ++= killed.iterator
      killed = savedKilled
      (res, newKilled)

    private val keepNuTypes = false

    private val completed = new collection.mutable.HashSet[Symbol]

    override def skipRecheck(sym: Symbol)(using Context): Boolean =
      completed.contains(sym)

    extension [T <: Tree](tree: T)
      def hasCCType: Boolean = cc.hasNuType(tree)
      def ccType(using Context): Type = cc.nuType(tree)

    private def captures(tree: Tree)(using Context): Refs =
      atPhase(checkCapturesPhase)(tree.ccType.deepCaptureSet.elems)

    extension (refs: Refs)
      private def footprint(using Context): Refs =
        def recur(elems: Refs, newElems: List[CaptureRef]): Refs = newElems match
          case newElem :: newElems1 =>
            val superElems = newElem.captureSetOfInfo.elems.filter: superElem =>
              !superElem.isMaxCapability && !elems.contains(superElem)
            recur(elems ++ superElems, newElems1 ++ superElems.toList)
          case Nil => elems
        val elems: Refs = refs.filter(!_.isMaxCapability)
        recur(elems, elems.toList)

    override def recheckIdent(tree: Ident, pt: Type)(using Context): Type =
      tree.tpe match
        case ref: CaptureRef if ref.isTrackableRef && !ref.isRootCapability =>
          if killed.contains(ref) then
            report.error(i"Use of killed variable ${tree} is forbidden.", tree.srcPos)
        case _ =>

      val used = tree.markedFree
      if !used.elems.isEmpty then
        val usedFootprint = used.elems.footprint
        for ref <- usedFootprint do
          val stripped = ref.stripReach.stripReadOnly.stripMaybe
          if killed.contains(stripped) then
            report.error(i"Use of ${tree} is forbidden.\nIt captures ${ref} which is killed.", tree.srcPos)
      super.recheckIdent(tree, pt)

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
            recheck(tree.rhs, WildcardType) // we infer!
          case _ =>
            recheck(tree.rhs, resType)
    end recheckValDef

    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Type =
      sym.info match
        case FunctionOrMethod(_, _) =>
        case _ =>
          // println(s"${sym.info} <- ${sym}, is not a method type!")

      val resTree = tree.tpt
      val paramRefs = tree.termParamss.flatten.flatMap(_.toCaptureRefs)

      inContext(linkConstructorParams(sym).withOwner(sym)):
        val resType = recheck(resTree) // totally unnecessary
        if tree.rhs.isEmpty || sym.isInlineMethod || sym.isEffectivelyErased
        then resType
        else
          val rhsType = recheck(tree.rhs, WildcardType) // we just discard what type the typer gave
          resTree match
            case _: InferredTypeTree =>
              inferDefDef(tree, sym, rhsType, paramRefs)
            case _ =>
              checkExplicitDefDef(tree, sym, resType, paramRefs)
    end recheckDefDef

    /**
     * Plan - do inference on the tree.tpt and return new inferred, but then
     * in FXSetup when constructing the new MethodType integrate the params
     * into the KillType()
     */
    def inferDefDef(tree: DefDef, sym: Symbol, resType: Type, params: List[CaptureRef])(using Context): Type =
      val killedParams =
        for param <- params if killed.contains(param)
        yield param.killTree

      if !killedParams.isEmpty then
        KillType(resType, killedParams)
      else resType
    end inferDefDef

    /**
     * Checks that explicitly given type accounts for all killed parameters,
     * and that the anything capturing a killed parameter is not returned
     */
    def checkExplicitDefDef(tree: DefDef, sym: Symbol, resType: Type, params: List[CaptureRef])(using Context): Type =
      val formalDead = resType match
        case KillType(_, refs) =>
          refs.flatMap(_.toCaptureRefs)
        case _ => Nil

      for param <- params do
        if killed.contains(param) then
          if formalDead.isEmpty then
            report.error(i"Parameter ${param} is killed in ${sym} but ${sym} has no kill annotation!",
            tree.srcPos)
          else if !formalDead.contains(param) then
            report.error(i"Kill set of ${sym} does not contain killed argument ${param}",
            tree.srcPos)

      val rhsCaptures = captures(tree.rhs).footprint
      for ref <- rhsCaptures do
        if formalDead.contains(ref) then
          report.error(i"Killed parameter ${ref} cannot be captured in method result expression.", tree.srcPos)
      resType
    end checkExplicitDefDef

    /**
     * If a function kills a value it does not do anything.
     * Open question: If function kills cap what does it do?
     *
     */
    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val appType = super.recheckApply(tree, pt)

      appType match
        case KillType(_, refs) =>
          val validRefs = refs.filter: ref =>
            ref.tpe match
              case tp: CaptureRef if tp.isTrackableRef && !tp.isRootCapability => true
              case _ => false
          val deadRefs = SimpleIdentitySet(validRefs.flatMap(_.toCaptureRefs)*).footprint
          for ref <- deadRefs do
            val currentOwner = ctx.owner // in future do role.dclSym like SepCheck
            ref.pathRootOrShared match
              case ref: TermRef =>
                val refOwner = ref.symbol.maybeOwner.enclosingMethodOrClass
                if (currentOwner.enclosingMethodOrClass.isProperlyContainedIn(refOwner)) then
                  report.error(i"Killing a non-local variable ${ref} is prohibited!", tree.srcPos)
              case _ =>
            killed += ref.stripReach.stripMaybe.stripReadOnly
        case _ =>
      appType.dropTopLevelKill
    end recheckApply

    override def recheckClosureBlock(mdef: DefDef, expr: Closure, pt: Type)(using Context): Type =
        val sym = mdef.symbol
        sym.ensureCompleted() // unnecessary

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
      val savedKilled = util.HashSet[CaptureRef]()
      killed.foreach(savedKilled += _)

      val tBranch = recheck(tree.thenp, pt)
      val tKilled = util.HashSet[CaptureRef]()
      killed.foreach(tKilled += _)
      killed = savedKilled

      val eBranch = recheck(tree.elsep, pt)
      tKilled.foreach(killed += _)
      tBranch | eBranch

    override def recheckMatch(tree: Match, pt: Type)(using Context): Type =
      val selectorType = recheck(tree.selector, pt)
      val typesAndKill =
        for cas <- tree.cases yield
          segment(recheckCase(cas, selectorType.widen, pt))

      typesAndKill.foreach(killed ++= _._2.iterator)
      val casesType = typesAndKill.map(_._1)
      TypeComparer.lub(casesType)

    override def recheckDef(tree: ValOrDefDef, sym: Symbol)(using Context): Type =
      try super.recheckDef(tree, sym)
      finally completed += sym

    // TODO better error messaging for this?
    override def checkConformsExpr(actual: Type, expected: Type, tree: tpd.Tree, addenda: Addenda)(using Context): Type =
      val act = super.checkConformsExpr(actual, expected, tree, addenda)
      act

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

    private var killed = util.HashSet[CaptureRef]()

    private var used = util.HashSet[CaptureRef]()

    // override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
    //   val (funtpe0, qualType) = tree.fun match
    //     case fun: Select =>
    //       val qualType = recheck(fun.qualifier, selectionProto(fun, WildcardType)).widenIfUnstable
    //       (recheckSelection(fun, qualType, fun.name, WildcardType), qualType)
    //     case _ =>
    //       (recheck(tree.fun), NoType)
    //   val funtpe1 = if tree.fun.symbol.originalSignaturePolymorphic.exists then tree.fun.tpe else funtpe0
    //   funtpe1.widen match

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      unit.tpdTree = setup.setupUnit(unit.tpdTree, this)
      super.checkUnit(unit)

  end EffectChecker
end CheckEffects

