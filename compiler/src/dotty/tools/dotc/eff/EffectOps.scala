package dotty.tools
package dotc
package eff

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*, Denotations.*
import ast.tpd, tpd.*
import cc.*
import CheckEffects.*
import Annotations.*

object KillType:
  import KillOps.*
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
end KillType

object KillOps:
  /**
   * Given a annotation, finds its killedElems from its tree (annot.tree).
   */
  extension (tree: Tree)
    def killedElems(using Context): List[Tree] = tree match
      case Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil) =>
        elems
      case _ =>
        Nil

  extension (tp: Type)
    def dropAllKill(using Context): Type =
      val tm = new TypeMap:
        def apply(t: Type) = t match
          case KillType(parent, _) =>
            apply(parent)
          case _ =>
            mapOver(t)
      tm(tp)

    def dropAllNotKill(using Context): Type =
      val tm = new TypeMap:
        def apply(t: Type) = t match
          case AnnotatedType(parent, annot) if !annot.symbol.isKill =>
            apply(parent)
          case _ =>
            mapOver(t)
      tm(tp)

    def dropTopLevelKill(using Context): Type =
      tp match
        case KillType(parent, killSet) => parent
        case _ => tp

    def getKilled(using Context): List[Tree] =
      tp match
        case KillType(_, killSet) => killSet
        case _ => Nil

    def isKillFun(using Context): Boolean =
      tp match
        case fntpe @ FunctionOrMethod(_, KillType(_, _)) => true
        case _ => false

    def isKillType(using Context): Boolean =
      tp match
        case KillType(_) => true
        case _ => false


  /**
   * Idea - if method type is a kill function, then
   * remove all non-parameter block-local refs and add a function self ref.
   */
  def avoidLocal(using Context) = new TypeOps.AvoidMap:
    def toAvoid(tp: NamedType): Boolean = true
    override def apply(tp: Type): Type =
      tp match
        case fntpe: MethodType if fntpe.isKillFun =>
          val killed = fntpe.getKilled.flatMap(_.toCaptureRefs)
          mapOver(fntpe)
        case _ => super.apply(tp)


  /**
   * Checks that the variables inside a kill annotation are well-formed
   * Well-formedness conditions:
   * 1. Must be non-empty
   * 2. No duplicates allowed e.g. no kill(f, f).
   * 3. Must be a capture trackable ref
   * 4. Must not be a special capability
   * 5. Kill annotation can only appear in (NOT YET CHECKED! TODO!)
   *  a) At top-level of explicit DefDef tpt
   *  b) As result type of a MethodType (e.g. A => B @kill(...) => C) should not be allowed
   */
  def checkWellformed(annot: Tree)(using Context): Unit =
    val killedElems = annot.killedElems
    if killedElems.isEmpty then
      report.error(i"Kill set of $annot may be empty.", annot.srcPos)
    else
      val killSet = util.HashSet[Symbol]()
      for elem <- killedElems do
        val sym = elem.symbol
        if !killSet.add(sym) then
          report.error(i"Kill set of $annot has a duplicate element $elem", annot.srcPos)
        elem.tpe match
          case ref: CaptureRef if ref.isTrackableRef =>
            if ref.isRootCapability then // hopefully only case that needs handling.
              report.error(i"Killed variable cannot be a root capability!", annot.srcPos)
          case _ if sym.isFuncSelfRef =>
          case _ =>
            report.error(i"Killed variable ${elem} is not a capability!", annot.srcPos)
  end checkWellformed
end KillOps

object EffectType:
  import EffOps.*

  def apply(tp: Type, usedRefs: List[Tree], killedRefs: List[Tree])(using Context): Type =
    val annotTree =
      Apply(
        New(getEffAnnot.typeRef,
          Typed(
            SeqLiteral(usedRefs, TypeTree(defn.AnyType)),
            TypeTree(defn.RepeatedParamClass.typeRef.appliedTo(defn.AnyType))
          ) :: Nil),
        Typed(
          SeqLiteral(killedRefs, TypeTree(defn.AnyType)),
          TypeTree(defn.RepeatedParamClass.typeRef.appliedTo(defn.AnyType))
        ) :: Nil
      )
    AnnotatedType(tp, Annotation(annotTree))

  def unapply(tp: Type)(using Context): Option[(Type, List[Tree], List[Tree])] =
    tp match
      case AnnotatedType(parent, annot) if annot.symbol.isEff =>
        Some(parent, annot.tree.usedElems, annot.tree.killedElems)
      case _ => None
end EffectType

object EffOps:
  extension (tree: Tree)
    def usedElems: List[Tree] = tree match
      case Apply(
        Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil), _
        ) => elems
      case _ => Nil

    // This is the same as KillOps.killedElems
    def killedElems: List[Tree] = tree match
      case Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil) => elems
      case _ => Nil

  extension (tp: Type)
    def dropAllEff(using Context): Type =
      val tm = new TypeMap:
        def apply(t: Type) = t match
          case EffectType(parent, _, _) =>
            apply(parent)
          case _ =>
            mapOver(t)
      tm(tp)

    def dropTopLevelEff(using Context): Type = tp match
      case EffectType(parent, _, _) => parent
      case tp => tp

  /**
   * Well-formedness conditions:1
   * 1. Both sets cannot be empty (one set can be empty)
   * 2. No duplicates allowed within each set
   * 3. All refs be a capture trackable ref
   * 4. All refs must not be a special capability
   */
  def checkWellformed(annot: Tree)(using Context): Unit =
    val killedElems = annot.killedElems
    val usedElems = annot.usedElems
    if (killedElems.isEmpty && usedElems.isEmpty) then
      report.error(i"Both the kill set and use set of $annot are empty!", annot.srcPos)
    else
      val killSet = util.HashSet[Symbol]()
      val useSet = util.HashSet[Symbol]()
      for kref <- killedElems
          uref <- usedElems
      do
        val krefSym = kref.symbol
        val urefSym = uref.symbol
        if (!killSet.add(krefSym)) then
          report.error(i"Kill set of $annot has a duplicate ref $kref", annot.srcPos)
        if (!useSet.add(urefSym)) then
          report.error(i"Use set of $annot has a duplicate ref $uref", annot.srcPos)
        kref.tpe match
          case ref: CaptureRef if ref.isTrackableRef =>
            if ref.isRootCapability then
              report.error(i"Killed variable $ref cannot be a root capability!", annot.srcPos)
          case _ =>
            report.error(i"Killed variable ${kref} is not a capability!", annot.srcPos)
        uref.tpe match
          case ref: CaptureRef if ref.isTrackableRef =>
            if ref.isRootCapability then
              report.error(i"Used variable $ref cannot be a root capability!", annot.srcPos)
          case _ =>
            report.error(i"Used variable $uref is not a capability!", annot.srcPos)
  end checkWellformed

end EffOps

