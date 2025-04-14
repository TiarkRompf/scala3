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

  extension (tree: Tree)
    /**
     * Given a annotation, finds its killedElems from its tree (annot.tree).
     */
    def killedElems(using Context): List[Tree] = tree match
      case Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil) =>
        elems
      case _ =>
        Nil

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

  extension (tp: Type)
    def dropAllKill(using Context): Type =
      val tm = new TypeMap:
        def apply(t: Type) = t match
          case EffectType(parent, _) =>
            apply(parent)
          case _ =>
            mapOver(t)
      tm(tp)

    /**
     * Drops all non-kill annotations
     */
    def dropAllNotKill(using Context): Type =
      val tm = new TypeMap:
        def apply(t: Type) = t match
          case AnnotatedType(parent, annot) if !annot.symbol.isKill =>
            apply(parent)
          case _ =>
            mapOver(t)
      tm(tp)

    def dropTopLevelKill(using Context): Type = // maybe recursively drop?
      tp match
        case EffectType(parent, killSet) => parent
        case _ => tp

    def getKilled(using Context): List[Tree] =
      tp match
        case EffectType(_, killSet) => killSet
        case _ => Nil

    def getKillAnnot(using Context): Option[Annotation] =
      tp match
        case AnnotatedType(parent, annot) if annot.symbol.isKill => Some(annot)
        case _ => None

    def isKillFun(using Context): Boolean =
      tp match
        case fntpe @ FunctionOrMethod(_, EffectType(_, _)) => true
        case _ => false

    def isEffType(using Context): Boolean =
      tp match
        case EffectType(_) => true
        case _ => false

  extension (ref: CaptureRef)
    def killTree(using Context): Tree =
      import ast.untpd
      untpd.Ident(ref.termSymbol.name).withType(ref)

  /**
   * Checks that the variables inside a kill annotation are well-formed
   * Well-formedness conditions:
   * 1. Must be non-empty
   * 2. No duplicates allowed e.g. no kill(f, f).
   * 3. Must be a capture trackable ref
   * 4. Must not be a special capability
   * 5. Kill annotation can only appear in (this one should be checked in setup phase)
   *  a) At top-level of explicit DefDef tpt
   *  b) As result type of a MethodType
   */
  def checkWellformed(annot: Tree)(using Context): Unit =
    val killedElems = annot.killedElems
    if killedElems.isEmpty then
      report.error(i"Kill set of $annot may be empty.", annot.srcPos)
    else
      val killSet = util.HashSet[Symbol]()
      for ref <- killedElems do
        val refSym = ref.symbol
        if !killSet.add(refSym) then
          report.error(i"Kill set of $annot has a duplicate ref $ref", annot.srcPos)
        ref.tpe match
          case ref: CaptureRef if ref.isTrackableRef =>
            if ref.isRootCapability then // hopefully only case that needs handling.
              report.error(i"Killed variable cannot be a root capability!", annot.srcPos)
          case _ =>
            report.error(i"Killed variable ${ref} is not a capability!", annot.srcPos)
  end checkWellformed

  object EffectType:
    def apply(tp: Type, refs: List[Tree])(using Context): Type =
      val annotTree =
        New(getKillAnnot.typeRef,
          Typed(
            SeqLiteral(refs, TypeTree(defn.AnyType)),
            TypeTree(defn.RepeatedParamClass.typeRef.appliedTo(defn.AnyType))) :: Nil)
      AnnotatedType(tp, Annotation(annotTree))

    def unapply(tp: Type)(using Context): Option[(Type, List[Tree])] =
      tp match
        case AnnotatedType(parent, annot) if annot.symbol.isKill =>
          Some(parent, annot.tree.killedElems)
        case _ => None

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
    EffectChecker(ctx, cc)

  class EffectChecker(ictx: Context, cc: CheckCaptures.CheckerAPI) extends Rechecker(ictx), FXCheckerAPI:
    import CheckEffects.*
    import cc.*

    private val killed = util.HashSet[CaptureRef]()

    private val keepNuTypes = false

    private val setup: FXSetupAPI = thisPhase.prev.asInstanceOf[FXSetup]

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
          println(s"${sym.info} <- ${sym}, is not a method type!")

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
     * into the EffectType()
     */
    def inferDefDef(tree: DefDef, sym: Symbol, resType: Type, params: List[CaptureRef])(using Context): Type =
      val killedParams =
        for param <- params if killed.contains(param)
        yield param.killTree

      if !killedParams.isEmpty then
        EffectType(resType, killedParams)
      else resType
    end inferDefDef

    /**
     * Checks that explicitly given type accounts for all killed parameters,
     * and that the anything capturing a killed parameter is not returned
     */
    def checkExplicitDefDef(tree: DefDef, sym: Symbol, resType: Type, params: List[CaptureRef])(using Context): Type =
      val formalDead = resType match
        case EffectType(_, refs) =>
          refs.flatMap(_.toCaptureRefs)
        case _ => Nil

      if !formalDead.isEmpty then
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
      end if
      resType
    end checkExplicitDefDef

    /**
     * If a function kills a value it does not do anything.
     * Open question: If function kills cap what does it do?
     */
    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val appType = super.recheckApply(tree, pt)

      appType match
        case EffectType(_, refs) =>
          val validRefs = refs.filter: ref =>
            ref.tpe match
              case tp: CaptureRef if tp.isTrackableRef && !tp.isRootCapability => true
              case _ => false
          val deadRefs = SimpleIdentitySet(validRefs.flatMap(_.toCaptureRefs)*).footprint
          for ref <- deadRefs do
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
          println(s"${tp} <- ${mdef.name.show}")
          recheckClosure(expr, pt, forceDependent = false)

        expr.setNuType(newTpe)
        newTpe
    end recheckClosureBlock

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

