package dotty.tools
package dotc
package eff

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*, Denotations.*
import ast.tpd, tpd.*
import transform.{PreRecheck, Recheck}
import annotation.{tailrec, threadUnsafe}
import cc.*
import CaptureSet.*
import Recheck.*
import NamerOps.linkConstructorParams
import util.SimpleIdentitySet
import Annotations.*

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

    def dropTopLevelKill(using Context): Type = // maybe recursively drop?
      tp match
        case EffectType(parent, killSet) => parent
        case _ => tp

    def getKilled(using Context): List[Symbol] =
      tp match
        case EffectType(_, killSet) => killSet.map(_.symbol)
        case _ => Nil

    def getKillAnnot(using Context): Option[Annotation] =
      tp match
        case AnnotatedType(parent, annot) if annot.symbol.isKill => Some(annot)
        case _ => None

  /**
   * Checks that the variables inside a kill annotation are well-formed
   * Well-formedness conditions:
   * 1. Must be non-empty
   * 2. Must be a capability - capturing type/retaining type/extends Capability
   * 3. Kill annotation can only appear at function return type - idk how to check this
   * 4. No duplicates allowed e.g. no kill(f, f).
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
        ref.tpe.widen match
          case RetainingType(_) => ()
          case CapturingType(_) => () // I don't think these should ever exist during typer phase
          case _ =>
            // println(ref.symbol.denot.info)
            // TODO figure out how to check if class extends capability or a
            // TODO find more capabilities identifying info for extends capability

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

  override def isRunnable(using Context) = true

  def newRechecker()(using Context): Rechecker =
    var unit = ctx.compilationUnit
    val ccPhase = checkCapturesPhase.asInstanceOf[CheckCaptures]
    val cc = ccPhase.checker match
      case null => assert(false, "Internal Error: Checker not assigned at CheckEffects!")
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
      val used = tree.markedFree
      if !used.elems.isEmpty then
        val usedFootprint = used.elems.footprint
        for ref <- usedFootprint do
          val stripped = ref.stripReach.stripReadOnly.stripMaybe
          if killed.contains(stripped) then
            report.error(i"Use of ${tree} is forbidden.\nIt captures ${ref} which is killed.", tree.srcPos)
      super.recheckIdent(tree, pt)

    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Type =
      super.recheckValDef(tree, sym)

    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Type =
      val (paramInfos, resInfo) = sym.info match
        case FunctionOrMethod(paramInfos, resInfo) => (paramInfos, resInfo)
        case _ => println(s"${sym.info} <- ${sym}, is not a method type!")
          assert(false)

      val resTree = tree.tpt
      val paramRefs = tree.termParamss.flatten.flatMap(_.toCaptureRefs)
      val formalDead = resTree.tpe match
        case EffectType(_, refs) =>
          refs.flatMap(_.toCaptureRefs)
        case _ => Nil

      // println(s"${sym.show}")
      // println(resInfo == resTree.tpe)
      // println(resInfo)
      // println(resTree.tpe)

      def accountsFor() =
        for param <- paramRefs do
          if killed.contains(param) then
            if formalDead.isEmpty then
              report.error(i"Parameter ${param} is killed in ${sym} but ${sym} has no kill annotation!",
              tree.srcPos)
            else if !formalDead.contains(param) then
              report.error(i"Kill set of ${sym} does not contain killed argument ${param}",
              tree.srcPos)

      inContext(linkConstructorParams(sym).withOwner(sym)):
        val resType = recheck(resTree)
        if tree.rhs.isEmpty || sym.isInlineMethod || sym.isEffectivelyErased
        then resType
        else
          val _ = recheck(tree.rhs, resType)
          resTree match
            case _: InferredTypeTree =>
              resInfo
            case _ =>
              accountsFor()
              resInfo

    // override def recheckFinish(tpe: Type, tree: Tree, pt: Type)(using Context): Type = tpe

    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val appType = super.recheckApply(tree, pt)
      appType match
        case EffectType(_, refs) =>
          val deadRefs = SimpleIdentitySet(refs.flatMap(_.toCaptureRefs)*).footprint
          for ref <- deadRefs do
            killed += ref.stripReach.stripMaybe.stripReadOnly
        case _ =>
      appType.dropTopLevelKill

    override def recheckDef(tree: ValOrDefDef, sym: Symbol)(using Context): Type =
      try super.recheckDef(tree, sym)
      finally completed += sym

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      unit.tpdTree = setup.setupUnit(unit.tpdTree, this)
      // denotPrinter().traverse(unit.tpdTree)
      super.checkUnit(unit)
      unit.tpdTree.removeAttachment(RecheckedTypes)

