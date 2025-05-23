package dotty.tools
package dotc
package eff

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*
import Annotations.Annotation
import ast.tpd, tpd.*
import transform.{MacroTransform, PreRecheck, Recheck}
import Recheck.*
import cc.*
import CheckEffects.*
import NamerOps.{methodType}

trait FXSetupAPI:
  def setupUnit(tree: Tree, checker: FXCheckerAPI)(using Context): Tree
end FXSetupAPI

object FXSetup:
  val name: String = "setupEff"
  val description: String = "prepare compilation unit for effect checking"
end FXSetup

class FXSetup extends PreRecheck, SymTransformer, FXSetupAPI:
  thisPhase =>
  override def phaseName: String = FXSetup.name
  override def description: String = FXSetup.description
  override def isRunnable(using Context): Boolean = super.isRunnable
  override def changesBaseTypes: Boolean = true
  override def transformSym(symd: SymDenotation)(using Context): SymDenotation = symd

  private def updateInfo(sym: Symbol, info: Type)(using Context) =
    sym.updateInfo(thisPhase, info, sym.flags)

  /*
  Sets up compilation unit for effect checking
  1. Gives inferred valdefs and defdefs LazyTypes
  2. TODO: checks that the kill set of a function is a subset of the function captures set + parameters capture set
  */
  class KillSetupTransformer(checker: CheckEffects.FXCheckerAPI) extends TreeMapWithPreciseStatContexts(cpy = cpyBetweenPhases):
    import checker.*
    import KillOps.*
    override def transform(tree: Tree)(using Context): Tree = tree match
      case tree @ DefDef(name, paramss, tpt, rhs) =>
        // todo handle parameterless functions (ExprType)
        val sym = tree.symbol
        // after postTyper all tpts should be TypeTrees, so should be ok
        // I also want this to fail if its not the case
        val forcedRes = tpt.asInstanceOf[TypeTree]
        if forcedRes.isInferred && !sym.isConstructor then
          val droppedRes = forcedRes.tpe.dropAllKill
          val newTree = super.transform(cpy.DefDef(tree)(name, paramss, tpt.withType(droppedRes),rhs))
          sym.info match
              // todo: maybe too powerful?
              // maybe only MethodType and PolyType?
              case fntpe @ FunctionOrMethod(params, resType) =>
                val newInfo = fntpe.derivedFunctionOrMethod(params, resType.dropAllKill)
                val updatedInfo = new LazyType:
                  def complete(denot: SymDenotation)(using Context): Unit =
                    assert(ctx.phase == thisPhase.next, i"$sym")
                    denot.info = newInfo
                    val newResType = recheckDef(newTree.asInstanceOf[DefDef], sym)
                    // TODO - instead of making new methodType, try to do something like integrateRT?
                    denot.info = methodType(sym.paramSymss, newResType, false)
                updateInfo(sym, updatedInfo)

              case exprType @ ExprType(resType) => // TODO write some tests for this
                val newInfo = exprType.derivedExprType(resType.dropAllKill)
                val updatedInfo = new LazyType:
                  def complete(denot: SymDenotation)(using Context): Unit =
                    assert(ctx.phase == thisPhase.next, i"$sym")
                    denot.info = newInfo
                    val newResType = recheckDef(newTree.asInstanceOf[DefDef], sym)
                    denot.info = newInfo.derivedExprType(newResType)
                updateInfo(sym, newInfo)
              case tp =>
                // println(s"${tp} <- ${sym.show}")
          newTree
        else
          super.transform(tree)
      case tree @ TypeApply(fn, args) =>
        // top level @kill should only exist on
        // function return type, so it should not
        // be possible to use as type apply?
        val droppedArgs = args.map: arg =>
          arg match
            case arg: TypeTree =>
              arg.withType(transform(arg).tpe.dropTopLevelKill)
            case _ => arg
        super.transform(cpy.TypeApply(tree)(fn, droppedArgs))
      case tree @ ValDef(name, tpt, rhs) =>
        val sym = tree.symbol
        if sym.exists && !sym.is(Param) && !sym.is(Module) then
          val forcedRes = tpt.asInstanceOf[TypeTree]
          if forcedRes.isInferred then
            // if (sym.info != forcedRes.tpe) then
            //   println("DEBUGGING INFO: VALDEF SYM.INFO != TPT.TPE")
            //   println(tree.show)
            //   println(sym.info)
            //   println(forcedRes.tpe)
            //   println("==================================================")
            val newTptTpe = forcedRes.tpe.dropAllKill
            val newTree = cpy.ValDef(tree)(
              name,
              tpt.withType(newTptTpe),
              transform(tree.rhs)
            )

            val newInfo = sym.info.dropAllKill // I think some cases exist where sym.info != forcedRes.tpe
            val updatedInfo = new LazyType:
              def complete(denot: SymDenotation)(using Context): Unit =
                assert(ctx.phase == thisPhase.next, i"$sym")
                denot.info = newInfo
                val newResType = recheckDef(newTree, sym)
                denot.info = newResType

            updateInfo(sym, updatedInfo)
            newTree
          else
            super.transform(tree)
        else
          super.transform(tree)
      // case Apply(fun, args) =>
      //   if !args.isEmpty then
      //     println(s"${args.head.tpe} <- at Setup")
      //     println(s"---------")
      //   super.transform(tree)
      case _ =>
        super.transform(tree)

  class FXSetupTransformer(checker: CheckEffects.FXCheckerAPI) extends TreeMapWithPreciseStatContexts(cpy = cpyBetweenPhases):
    import checker.*
    import EffOps.*
    override def transform(tree: Tree)(using Context): Tree =
      tree

  def setupUnit(tree: Tree, checker: FXCheckerAPI)(using Context): Tree =
    if onlyEffCheckKill then
      atPhase(thisPhase)(KillSetupTransformer(checker).transform(tree))
    else
      atPhase(thisPhase)(FXSetupTransformer(checker).transform(tree))

end FXSetup