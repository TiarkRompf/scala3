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
  2. TODO: checks that the kill set of a function is a subset of the function captures set + parameters capture set - this can probably
  be done by looking at the capturedVars of a DefDef
  */
  class KillSetupTransformer(checker: CheckEffects.FXCheckerAPI) extends TreeMapWithPreciseStatContexts(cpy = cpyBetweenPhases):
    import checker.*
    import cc.*
    import KillOps.*

    def checkExplicitTT(tree: TypeTree)(using Context): Unit =
      val checkTraverser = new TypeTraverser:
        def traverse(tp: Type): Unit = tp match
          case KillType(parent, refs) =>
            val crefs = try
              atCC(refs.filterNot(_.symbol.isFuncSelfRef).flatMap(toCapabilities))
            catch
              case ex: IllegalCaptureRef =>
                report.error(em"Illegal capture reference: ${ex.getMessage} in kill set of ${tree}", tree.srcPos)
                Nil
            for ref <- crefs do
              if atCC(ref.captureSetOfInfo.elems.isEmpty)
                  && !ref.coreType.derivesFrom(defn.Caps_Capability) then

                val isPolyParam = ref.coreType match
                  case ref: TermRef =>
                    ref.typeSymbol.isTypeParam
                  case ref: TermParamRef =>
                    ref.typeSymbol.isTypeParam
                  case _ => false
                // we check deep capture set since
                // for tuples, only the deep capture set has a capability.
                // probably special case this for common data structures like tuples and lists.
                // instead of checking in general? who knows.
                if !isPolyParam && atCC(ref.coreType.deepCaptureSet.elems.isEmpty) then
                  report.error(em"${ref} cannot be killed since its capture set is empty!", tree.srcPos)
            traverseChildren(parent)
          case defn.RefinedFunctionOf(mt) =>
            /**
             * We ignore the parent of refined functions because weird things happen. In particular, this
             * causes the body parameter in withFile in the file example to break by saying that
             * the capture set of c: f.isClosed^ is empty, since it gives it underlying type of Nothing.
             * I don't know why this happens but it should be okay to ignore the parent for now since
             * we really only care about the method type anyways.
             */
            traverseChildren(mt)
          case _ => traverseChildren(tp)
        end traverse
      checkTraverser.traverse(tree.tpe)
    end checkExplicitTT

    override def transform(tree: Tree)(using Context): Tree =
      val transformedTree = tree match
        case tree : TypeTree =>
          if !tree.isInferred then
            checkExplicitTT(tree)
          tree
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
            checkExplicitTT(forcedRes)
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
              checkExplicitTT(forcedRes)
              super.transform(tree)
          else
            super.transform(tree)
        case _ =>
          super.transform(tree)
      transformedTree.setCCType(tree.ccType)
      transformedTree
    end transform

  class FXSetupTransformer(checker: CheckEffects.FXCheckerAPI) extends TreeMapWithPreciseStatContexts(cpy = cpyBetweenPhases):
    import checker.*
    import EffOps.*
    override def transform(tree: Tree)(using Context): Tree =
      tree

  def setupUnit(tree: Tree, checker: FXCheckerAPI)(using Context): Tree =
    if onlyEffCheckKill then
      // val newTree = addRecheckedTypes(tree)
      atPhase(thisPhase)(KillSetupTransformer(checker).transform(tree))
    else
      atPhase(thisPhase)(FXSetupTransformer(checker).transform(tree))

end FXSetup