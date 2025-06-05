package dotty.tools
package dotc
package eff

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*, Denotations.*
import ast.tpd, tpd.*
import cc.*, Capabilities.*
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
   * Given KillType(..., refs),
   * returns (whether refs has self ref, refs without self ref)
   */
  def cleanSelfRef(refs: List[Tree])(using Context): (Boolean, List[Tree]) =
    var killsSelf = false
    val cleaned = refs.filterConserve { ref =>
      if (ref.symbol.isFuncSelfRef) then
        killsSelf = true
        false
      else true
    }
    (killsSelf, cleaned)
  end cleanSelfRef

  /**
   * Idea - if method type is a kill function, then
   * remove all non-parameter block-local refs and add a function self ref.
   *
   * We do NOT want the default avoidance behavior for the kill annotation because what will happen is by
   * default, avoidance replaces local TermRefs with their types, but then the problem is that replaced type
   * is not a capability!
   *
   * The case for applied types is mainly because of how refined types work
   * A refined function is something like RefinedType(AppliedType(), apply, MethodType(...))
   * So the MethodType case will handle that, but the AppliedType still needs work, since the
   * applied type will be the non-dependent function type, but it will still have the kill annotation
   * in the last argument.
   *
   * The other way to avoid doing this is probably to drop all kill annotations in the parent
   * of any refined function type after mapping over it in saturate. This solution means that the
   * AppliedType is not going to be aligned with the refined function type, which could be bad so for now
   * I'm going with handling AppliedTypes in avoidance.
   */
  def avoidKill(tp: Type, symsToAvoid: => List[Symbol])(using Context): Type =
    lazy val forbidden = symsToAvoid.toSet
    val escapeMap = new TypeOps.AvoidMap:
      def toAvoid(tp: NamedType): Boolean =
        val sym = tp.symbol
        forbidden.contains(sym)
      override def apply(tp: Type): Type = tp match
        case fntpe: MethodType if fntpe.isKillFun =>
          val KillType(resType, killedRefs) = fntpe.resultType: @unchecked
          val (alreadyKillsSelf, cleanedRefs) = cleanSelfRef(killedRefs)
          var needsSelfRef = false

          val goodRefs =
            cleanedRefs.flatMap(_.toCapabilities)
            .filter { ref => ref match
              case tp: TermRef if toAvoid(tp) =>
                needsSelfRef = variance > 0
                false
              case _ => true
            }.map(_.refTree) // TODO: figure out way to avoid using .refTree here

          val updatedRefs =
            if alreadyKillsSelf || needsSelfRef then
              makeFuncSelfRef :: goodRefs
            else goodRefs

          fntpe.derivedLambdaType(
            paramInfos = atVariance(-variance)(fntpe.paramInfos.mapConserve(apply)),
            resType =
              if updatedRefs.isEmpty then apply(resType)
              else
                KillType(apply(resType), updatedRefs)
          )
        case tpe @ AppliedType(parent, targs) if defn.isFunctionNType(tpe) && targs.last.isKillType =>
          // an AppliedType will be a function of (targs.init) => targs.last
          val KillType(resType, killedRefs) = targs.last: @unchecked
          val (alreadyKillsSelf, cleanedRefs) = cleanSelfRef(killedRefs)
          var needsSelfRef = false
          val goodRefs =
            cleanedRefs.flatMap(_.toCapabilities)
            .filter { ref => ref match
              case tp: TermRef if toAvoid(tp) =>
                needsSelfRef = variance > 0
                false
              case _ => true
            }.map(_.refTree) // TODO: figure out way to avoid using .refTree here

          val updatedRefs =
            if alreadyKillsSelf || needsSelfRef then
              makeFuncSelfRef :: goodRefs
            else goodRefs

          tpe.derivedAppliedType(
            apply(parent),
            (atVariance(-variance)(targs.init.mapConserve(apply))) :+
            {
              if updatedRefs.isEmpty then apply(resType)
              else
                KillType(apply(resType), updatedRefs)
            }
          )
        case CapturingType(parent, refs) =>
          atCC(mapCapturingType(tp, parent, refs, variance))

        case tp: TypeVar if mapCtx.typerState.constraint.contains(tp) => // copied from avoid
          val lo = TypeComparer.instanceType(
            tp.origin,
            fromBelow = variance > 0 || variance == 0 && tp.hasLowerBound,
            tp.widenPolicy)(using mapCtx)
          val lo1 = apply(lo)
          if (lo1 ne lo) lo1 else tp

        case _ => super.apply(tp)
      end apply
    end escapeMap
    escapeMap(tp)
  end avoidKill

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
          case ref: Capability if ref.isTrackableRef =>
            if ref.isTerminalCapability then // hopefully only case that needs handling.
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
          case ref: Capability if ref.isTrackableRef =>
            if ref.isTerminalCapability then
              report.error(i"Killed variable $ref cannot be a root capability!", annot.srcPos)
          case _ =>
            report.error(i"Killed variable ${kref} is not a capability!", annot.srcPos)
        uref.tpe match
          case ref: Capability if ref.isTrackableRef =>
            if ref.isTerminalCapability then
              report.error(i"Used variable $ref cannot be a root capability!", annot.srcPos)
          case _ =>
            report.error(i"Used variable $uref is not a capability!", annot.srcPos)
  end checkWellformed

end EffOps

