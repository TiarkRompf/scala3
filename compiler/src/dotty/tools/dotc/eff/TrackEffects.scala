package dotty.tools
package dotc
package eff

import core.*
import Phases.*, DenotTransformers.*, Flags.*
import Symbols.*, Contexts.*, Types.*, ContextOps.*, Decorators.*, SymDenotations.*
import transform.{Recheck, PreRecheck, CapturedVars}
import Recheck.*
import Types.*, StdNames.*, Denotations.*
import ast.{tpd, untpd, Trees}
import annotation.tailrec
import Annotations.Annotation
import NamerOps.linkConstructorParams

object TrackEffects:
  val name: String = "eff"
  val description: String = "effect tracking"

  trait CheckerAPI:
    /** Complete symbol info of a val or a def */

    extension [T <: tpd.Tree](tree: T)

      /** Set new type of the tree if none was installed yet. */
      def setNuType(tpe: Type): Unit

      /** The new type of the tree, or if none was installed, the original type */
      def nuType(using Context): Type

      /** Was a new type installed for this tree? */
      def hasNuType: Boolean
  end CheckerAPI

end TrackEffects

class TrackEffects extends Recheck, SymTransformer:
  thisPhase =>

  import tpd.*
  import TrackEffects.*

  override def phaseName: String = TrackEffects.name
  override def description: String = TrackEffects.description
  override def isRunnable(using Context): Boolean = true

  def newRechecker()(using Context): Rechecker = EffectTracker(ctx)

  class EffectTracker(ictx: Context) extends Rechecker(ictx), CheckerAPI:

    /** Given a list of types, composes the possible effects from left to right.
     * Future optimization - use a list buffer?
     */
    private def composeEffects(tpes: List[Type])(using Context): Option[EffectAnnotation] =
      tpes.foldLeft(None) ((effect, tpe) =>
        (effect, tpe) match
          case (Some(effect), EffectType(_)) => Some(effect |> EffectAnnotation())
          case (Some(effect), _) => Some(effect)
          case (None, EffectType(_)) => Some(EffectAnnotation())
          case (None, _) => None
      )

    override def recheckValDef(tree: tpd.ValDef, sym: Symbol)(using Context): Type =
      val tpdValDef = super.recheckValDef(tree, sym)
      // println(i"${tree} = ${tpdValDef}")
      tpdValDef

    override def recheckDefDef(tree: tpd.DefDef, sym: Symbols.Symbol)(using Context): Type =
      // if tree.name.show == "$anonfun" then
      //   println(s"${tree.tpt}")
      //   println(s"${tree.tpt.show}")
      //   println(s"${tree.tpt.tpe}")
      // println(s"Tree = ${tree.name.show}, Return Type = ${tree.tpt}")
      val isPrimordial = tree.name.show match
        case "open" | "write" | "close" => true
        case _ => false
      inContext(linkConstructorParams(sym).withOwner(sym)):
        val resType = if sym.isRealMethod then recheck(tree.tpt) else tree.tpt.tpe // if eta expansion then...
        if tree.rhs.isEmpty || sym.isInlineMethod || sym.isEffectivelyErased || isPrimordial
        then resType
        else
          val rhsType = recheck(tree.rhs, resType)
          (resType.isEffType, rhsType.isEffType) match // put this in checkConforms
            case (true, true) | (false, false) =>
              println(i"Method '${tree.name}' returns type ${rhsType}")
              rhsType
            case (true, false) =>
              report.error(i"Method '${tree.name}' should be effectful when its actual type '${rhsType}' may not be.",
              tree.srcPos)
              rhsType
            case (false, true) =>
              report.error(i"Method '${tree.name}' should be pure when its actual type '${rhsType}' may not be.",
              tree.srcPos)
              rhsType

    override def recheckApply(tree: tpd.Apply, pt: Type)(using Context): Type =
      val (funtpe0, qualType) = tree.fun match // TODO: check if type checking function type is effectful
        case fun: Select =>
          val qualType = recheck(fun.qualifier, selectionProto(fun, WildcardType)).widenIfUnstable
          (recheckSelection(fun, qualType, fun.name, WildcardType), qualType)
        case _ =>
          (recheck(tree.fun), NoType)
      val extracted = funtpe0 match
        case TermRef(prefix, _) => prefix
        case _ => funtpe0
      // below line doesn't do anything as of rn
      val funtpe1 = if tree.fun.symbol.originalSignaturePolymorphic.exists then tree.fun.tpe else funtpe0
      funtpe1.widen match
        case fntpe1: MethodType =>
          val fntpe = prepareFunction(fntpe1, tree.fun.symbol) // also doesn't do anything
          assert(fntpe.paramInfos.hasSameLengthAs(tree.args))
          val formals = fntpe.paramInfos
          def recheckArgs(args: List[Tree], formals: List[Type], prefs: List[ParamRef]): List[Type] = args match
            case arg :: args1 =>
              val argType = recheckArg(arg, normalizeByName(formals.head))
              val formals1 =
                if fntpe.isParamDependent
                then formals.tail.map(_.substParam(prefs.head, argType))
                else formals.tail
              argType :: recheckArgs(args1, formals1, prefs.tail)
            case Nil =>
              assert(formals.isEmpty)
              Nil
          val argTypes = recheckArgs(tree.args, formals, fntpe.paramRefs)
          val resType = recheckApplication(tree, qualType, fntpe, argTypes)
          val totalEffect = composeEffects((extracted :: argTypes) :+ resType)
          if totalEffect.isDefined then deriveEffectType(resType) else resType
        case tp =>
          assert(false, i"unexpected type of ${tree.fun}: $tp")

    /**
      * Checks each stat in block if has effect, composes them into one effect which is boxed in option
      */
    private def checkStatEffects(stats: List[Tree])(using Context): Option[EffectAnnotation] =
      @tailrec def traverse(stats: List[Tree], acc: List[Type])(using Context): List[Type] = stats match
        case (imp: Import) :: rest =>
          traverse(rest, acc)(using ctx.importContext(imp, imp.symbol))
        case stat :: rest =>
          traverse(rest, recheck(stat) :: acc)
        case Nil =>
          acc.reverse

      composeEffects(traverse(stats, List[Type]()))
    end checkStatEffects

    private def checkBlockEffects(stats: List[Tree], expr: Tree)(using Context): Type =
      val effect = checkStatEffects(stats)
      val exprType = recheck(expr)
      if effect.isDefined then deriveEffectType(exprType) else exprType
    end checkBlockEffects

    override def recheckBlock(tree: tpd.Block, pt: Type)(using Context): Type = tree match
      case Block(Nil, expr: Block) => recheckBlock(expr, pt)
      case Block((mdef : DefDef) :: Nil, closure: Closure) =>
        recheckClosureBlock(mdef, closure.withSpan(tree.span), pt)
      case Block(stats, expr) => checkBlockEffects(stats, expr)

    /**
      * Currently, if last expression of method is effectful e.g. def f() = open()
      * Then type will be inferred in Typer to be effectful
      * If last expression is not effectful, even if method is effectful, will be inferred to be pure
      */
    override def recheckTypeTree(tree: tpd.TypeTree)(using Context): Type = tree.tpe match
      case EffectType(parent, _) if tree.isInferred => parent
      case _ => tree.tpe


    private val setup: setupAPI = thisPhase.prev.asInstanceOf[Setup]
    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      // setup.setupUnit(unit.tpdTree, this)
      // recheck(addRecheckedTypes(unit.tpdTree))
      super.checkUnit(unit)

end TrackEffects
