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

  case class KillAnnotation(refs: SimpleIdentitySet[Symbol])(annot: Annotation) extends Annotation:
    def tree(using Context): Tree = annot.tree

  trait FXCheckerAPI:
    def recheckDef(tree: ValOrDefDef, sym: Symbol)(using Context): Type

end CheckEffects


class CheckEffects extends Recheck:
  thisPhase =>

  import CheckEffects.*

  override def phaseName: String = CheckEffects.name

  override def description: String = CheckEffects.description

  override def isRunnable(using Context) = true

  def newRechecker()(using Context): Rechecker =
    var unit = ctx.compilationUnit
    val ccTypes = unit.tpdTree.getAttachment(RecheckedTypes).getOrElse(
      assert(false, "There should be new types after capture checking!")
    )
    EffectChecker(ctx, ccTypes)

  class EffectChecker(ictx: Context, ccTypes: util.EqHashMap[Tree, Type]) extends Rechecker(ictx), FXCheckerAPI:
    import CheckEffects.*

    private val killedSyms = util.HashSet[Symbol]()

    private val keepNuTypes = true

    private val setup: FXSetupAPI = thisPhase.prev.asInstanceOf[FXSetup]

    extension[T <: Tree](tree: T)
      def ccType =
        val ntpe = ccTypes.lookup(tree)
        if ntpe != null then ntpe else tree.tpe

    override def recheckDefDef(tree: tpd.DefDef, sym: Symbol)(using Context): Type =
      EffectType(sym.info, Nil)

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      unit.tpdTree = setup.setupUnit(unit.tpdTree, this)
      denotPrinter().traverse(unit.tpdTree)
      super.checkUnit(unit)
      unit.tpdTree.removeAttachment(RecheckedTypes)

