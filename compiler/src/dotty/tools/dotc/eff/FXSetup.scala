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

  override def isRunnable(using Context): Boolean = true
  override def changesBaseTypes: Boolean = true

  override def transformSym(symd: SymDenotation)(using Context): SymDenotation = symd

  private def updateInfo(sym: Symbol, info: Type)(using Context) =
    // toBeUpdated += sym
    sym.updateInfo(thisPhase, info, sym.flags)
    // toBeUpdated -= sym

  class SetupTransformer(checker: CheckEffects.FXCheckerAPI) extends TreeMapWithPreciseStatContexts(cpy = cpyBetweenPhases):
    override def transform(tree: Tree)(using Context): Tree = tree match
      case tree @ DefDef(name, paramss, tpt, rhs) =>
        val sym = tree.symbol
        // after postTyper all tpts should be TypeTrees, so should be ok
        // I also want this to fail at run-time if its not the case
        val forcedRes = tpt.asInstanceOf[TypeTree]
        if forcedRes.isInferred && !sym.isConstructor then
          sym.info match
              // todo: maybe too powerful?
              // maybe only MethodType and PolyType?
              case fntpe @ FunctionOrMethod(params, resType) =>
                val newInfo = fntpe.derivedFunctionOrMethod(params, resType.dropAllKill)
                val updatedInfo = new LazyType:
                  def complete(denot: SymDenotation)(using Context): Unit =
                    assert(ctx.phase == thisPhase.next, i"$sym")
                    denot.info = newInfo
                    denot.info = newInfo.derivedFunctionOrMethod(params, checker.recheckDef(tree, sym))
                    // checker.recheckDef(tree, sym)
                    // denot.info = fntpe
                    // if (sym.isAnonymousFunction) then
                    //   // println(s"${sym.show}")
                    //   val nymph = newInfo.asInstanceOf[MethodType].resType.stripAnnots
                    //   val fres = forcedRes.tpe.stripAnnots
                    //   //println(fres.asInstanceOf[TypeRef].prefix.asInstanceOf[TermRef].designator)

                    //   // println(fres)
                    //   // println(nymph)
                    //   // println(fres <:< nymph)
                    //   // println(nymph <:< fres)
                    //   // // println(s"${newInfo.asInstanceOf[MethodType].resType <:< forcedRes.tpe}")
                    //   // // println(s"${forcedRes.tpe <:< newInfo.asInstanceOf[MethodType].resType}")
                    //   // println("--------------------------------------------------------")
                updateInfo(sym, updatedInfo)
              case tp =>
                println(s"${tp} <- ${sym.show}")
          val droppedRes = forcedRes.tpe.dropAllKill
          super.transform(cpy.DefDef(tree)(name, paramss, tpt.withType(droppedRes),rhs))
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

        cpy.TypeApply(tree)(transform(fn), droppedArgs)
      case tree @ ValDef(name, tpt, rhs) =>
        val sym = tree.symbol
        if sym.exists && !sym.is(Param) then
          assert(sym.info == tree.tpt.tpe)
          val newInfo = tree.tpt.tpe.dropTopLevelKill
          updateInfo(sym, newInfo)

          cpy.ValDef(tree)(
            name,
            tpt.withType(newInfo),
            transform(tree.rhs)
          )
        else
          super.transform(tree)
      case _ =>
        super.transform(tree)

  def setupUnit(tree: Tree, checker: FXCheckerAPI)(using Context): Tree =
    atPhase(thisPhase)(SetupTransformer(checker).transform(tree))
end FXSetup