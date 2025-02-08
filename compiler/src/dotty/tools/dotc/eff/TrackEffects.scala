package dotty.tools
package dotc
package eff

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*
import transform.{Recheck, PreRecheck, CapturedVars}
import Recheck.*
import Types.*, StdNames.*, Denotations.*
import Decorators.i
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

  import ast.tpd.*
  import TrackEffects.*

  override def phaseName: String = TrackEffects.name
  override def description: String = TrackEffects.description
  override def isRunnable(using Context): Boolean = true

  def newRechecker()(using Context): Rechecker = EffectTracker(ctx)

  class EffectTracker(ictx: Context) extends Rechecker(ictx), CheckerAPI: // maybe extend CheckerAPI as well

    override def keepNuTypes(using Context): Boolean = false // probably unnecessary

    // override def recheckIdent(tree: tpd.Ident, pt: Type)(using Context): Type = {
    //   // tree.name.show match {
    //   //   case "open" | "write" | "close" =>
    //   //     EffectType(tree.tpe, IO)
    //   //   case _ =>
    //   //     EffectType(tree.tpe, Bot)
    //   // }
    //   //println(s"tree.tpe = ${tree.tpe}, widen = ${tree.tpe.widen}")
    //   // val openTpe = tree.tpe.widen
    //   tree.nuType
    // }

    override def recheckApply(tree: tpd.Apply, pt: Type)(using Context): Type = {
      // println(s"${tree.show} = ${tree.tpe}")
      // println(s"${tree.nuType}")
      // val extractedArgs = argTypes.map(_.extract)
      // val appType = super.recheckApplication(tree, NoType, fntpe, extractedArgs)

      // println(s"funtpe = ${funtpe}")

      val tpdApp = super.recheckApply(tree, pt)
      // println(tpdApp)
      tpdApp
    }

    override def recheckDefDef(tree: tpd.DefDef, sym: Symbols.Symbol)(using Context): Type = {
    //  println(s"Tree = ${tree.name.show}, Return Type = ${tree.tpt.tpe}, Type = ${tree.tpe}")
      // tree.tpt.tpe match {
      //   case AnnotatedType(parent, annot) =>
      //     println(annot.symbol.name)
      //     if (annot.symbol.name.show == "eff") {
      //       println("asdf")
      //     }
      //     // annot match {
      //     //   case scala.annotation.retainsCap() => println(s"CAPTURE = ${annot.tree.tpe.show}")
      //     //   case _ => println(annot.tree.tpe.show)
      //     // }
      //   case _ => ()
      // }
      val isPrimordial = tree.name.show match {
        case "open" | "write" | "close" => true
        case _ => false
      }
      inContext(linkConstructorParams(sym).withOwner(sym)):
        val resType = recheck(tree.tpt)
        if tree.rhs.isEmpty || sym.isInlineMethod || sym.isEffectivelyErased || isPrimordial
        then resType
        else
          val rhsType = recheck(tree.rhs, resType)
          if tree.tpt.asInstanceOf[TypeTree].isInferred then
            println(i"Method '${tree.name}' has type ${rhsType}")
            rhsType
          // safe cast since tpd.DefDef
          // note that idk if type was inferred => can just assume rhsType is sound
          else
          (resType.isEffType, rhsType.isEffType) match {
            case (true, true) | (false, false) =>
              println(i"Method '${tree.name}' has type ${rhsType}")
              rhsType
            case (true, false) =>
              report.error(i"Method '${tree.name}' is effectful when its actual type '${rhsType}' may not be.",
              tree.srcPos)
              rhsType
            case (false, true) =>
              report.error(i"Method '${tree.name}' is pure when its actual type '${rhsType}' may not be.",
              tree.srcPos)
              rhsType
          }
    }

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

      val statTypes = traverse(stats, List[Type]())

      import scala.collection.mutable.ListBuffer
      statTypes.foldLeft(None) ((acc: Option[EffectAnnotation], tpe) => // change to monadic later?
        (acc, tpe) match {
          case (Some(effect), EffectType(_)) => Some(effect |> EffectAnnotation())
          case (Some(effect), _) => Some(effect)
          case (None, EffectType(_)) => Some(EffectAnnotation())
          case (None, _) => None
        }
      )
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


    private val setup: setupAPI = thisPhase.prev.asInstanceOf[Setup]
    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      // setup.setupUnit(unit.tpdTree, this)
      // recheck(addRecheckedTypes(unit.tpdTree))
      super.checkUnit(unit)

end TrackEffects
