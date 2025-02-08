package dotty.tools
package dotc
package eff

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*
import transform.PreRecheck
import ast.tpd, tpd.*
import TrackEffects.CheckerAPI

trait setupAPI:
  /**
   * For now, just traverses tree and changes every annotated type with eff to EffectAnnotation
   */
  def setupUnit(tree: Tree, checker: CheckerAPI)(using Context): Unit

object Setup:
  val name: String = "setupEff"
  val description: String = "prepare compilation unit for effect tracking"

class Setup extends PreRecheck, SymTransformer, setupAPI:
  thisPhase =>

  override def phaseName: String = Setup.name
  override def description: String = Setup.description
  override def isRunnable(using Context): Boolean = true

  def transformSym(sym: SymDenotation)(using Context): SymDenotation = sym

  extension (tp: Type)
    def isEffType(using Context): Boolean =
      tp match {
        case AnnotatedType(parent, annot) if annot.symbol.isEff => true
        case _ => false
      }

  extension (sym: Symbol)
      def isEff(using Context): Boolean =
        sym.name.show == "eff"

  /**
    * Maps every tree having type AnnotatedType(parent, annot) with annot.symbol.name.show == "eff"
    * to AnnotatedType(parent, EffectAnnotation(...)) in nuTypes map
    */
  def setupTraverser(checker: CheckerAPI) = new TreeTraverserWithPreciseImportContexts:
    import checker.*

    private def transformTree(tree: tpd.Tree)(using Context): Unit = {
      tree.tpe match {
        case AnnotatedType(parent, annot) if annot.symbol.isEff =>
          tree.setNuType(AnnotatedType(parent, EffectAnnotation()))
        case _ =>
          tree.setNuType(tree.tpe)
      }
    }

    def traverse(tree: Tree)(using Context): Unit =
      tree match {
        case tree: Ident =>
          if (tree.name.show == "open") then
            println(tree.tpe.widen)
        case tree: DefDef =>
          if (tree.name.show == "open") then
            println(s"At method decl = ${tree.tpt.tpe}")
        case _ => ()
      }
      transformTree(tree)
      traverseChildren(tree)
      tree match {
        case tree: Ident =>
          if (tree.name.show == "open") then
            println(tree.nuType)
        case tree: DefDef =>
          if (tree.name.show == "open") then
            println(s"At method decl = ${tree.tpt.nuType}")
        case _ => ()
      }
        // case tree @ DefDef(_, paramss, tpt, _) =>
        //   val meth = tree.symbol
        //   inContext(ctx.withOwner(meth)) {
        //     paramss.foreach(traverse)
        //     transformTree(tpt)
        //     traverse(tree.rhs)
        //   }

        // case tree @ ValDef(_, tpt, _) =>
        //   val sym = tree.symbol
        //   val defCtx = if sym.isOneOf(TermParamOrAccessor) then ctx else ctx.withOwner(sym)
        //   inContext(defCtx):
        //     transformTree(tpt)
        //     traverse(tree.rhs)

        // case tree: Ident => transformTree(tree)

  def setupUnit(tree: tpd.Tree, checker: CheckerAPI)(using Context): Unit = {
    setupTraverser(checker).traverse(tree)(using ctx.withPhase(thisPhase))
  }

end Setup

