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
import NamerOps.methodType
import typer.SigmaOps.*

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

  /**
  Sets up compilation unit for effect checking
  1. Drops all inferred kill annotations
  2. Gives LazyTypes to symbols relating to method and val definitions in preparation
     for inference.

    TODO - handle inline methods properly, right now we just
    drop all kill annotations from inline methods.
  */
  class KillSetupTransformer(checker: CheckEffects.FXCheckerAPI) extends TreeMapWithPreciseStatContexts:
    import checker.*
    import cc.*
    import KillOps.*

    /**
     * Checks explicitly given types for the following conditions on kill effect:
     * 1. Must be a capability with a non-empty capture set (TODO: relax this?).
     * 2. Cannot be a pure type variable
     * 3. Cannot be a object field (this is heuristically checked) unless path derives from Sigma
     */
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
                // probably special case this for common data uctures like tuples and lists.
                // instead of checking in general? who knows.
                if isPolyParam then
                  report.error(em"Term ${ref} cannot be killed since it is boxed!", tree.srcPos)
                if atCC(ref.coreType.deepCaptureSet.elems.isEmpty) then
                  report.error(em"${ref} cannot be killed since its capture set is empty!", tree.srcPos)
              else
                ref.coreType match
                  case TermRef(inner: TermRef, _) if !inner.widen.isSigma =>
                    report.error(em"Term ${ref} cannot be killed as it is an object field!", tree.srcPos)
                  case _ =>
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
        case tree: TypeTree =>
          if tree.isInferred then
            tree.withType(tree.tpe.dropAllKill)
          else
            checkExplicitTT(tree)
            tree

        case tree @ DefDef(name, paramss, tpt, rhs) =>
          val sym = tree.symbol
          // after postTyper all tpts should be TypeTrees, so should be ok
          // I also want this to fail if its not the case
          val forcedRes = tpt.asInstanceOf[TypeTree]
          // the isEmpty case shouldn't matter
          val shouldNotCheckRhs = tree.rhs.isEmpty || sym.isInlineMethod || sym.isEffectivelyErased

          val newTree =
            if shouldNotCheckRhs then
              if forcedRes.isInferred then
                sym.info match
                  case fntpe @ FunctionOrMethod(params, resType) =>
                    val newInfo = fntpe.derivedFunctionOrMethod(params, resType.dropAllKill)
                    updateInfo(sym, newInfo)
                  case _ =>
              cpy.DefDef(tree)(tpt = transform(forcedRes))
            else
              super.transform(tree).asInstanceOf[DefDef]

          if !shouldNotCheckRhs && forcedRes.isInferred && !sym.isConstructor then sym.info match
            // todo: maybe too powerful?
            // maybe only MethodType and PolyType?
            case fntpe @ FunctionOrMethod(params, resType) =>
              val newInfo = fntpe.derivedFunctionOrMethod(params, resType.dropAllKill)
              val updatedInfo = new LazyType:
                def complete(denot: SymDenotation)(using Context): Unit =
                  assert(ctx.phase == thisPhase.next, i"$sym")
                  denot.info = newInfo
                  val newResType = recheckDef(newTree, sym)
                  // TODO - instead of making new methodType, try to do something like integrateRT?
                  denot.info = methodType(sym.paramSymss, newResType, false)
              updateInfo(sym, updatedInfo)

            case exprType @ ExprType(resType) => // TODO write some tests for this
              val newInfo = exprType.derivedExprType(resType.dropAllKill)
              val updatedInfo = new LazyType:
                def complete(denot: SymDenotation)(using Context): Unit =
                  assert(ctx.phase == thisPhase.next, i"$sym")
                  denot.info = newInfo
                  val newResType = recheckDef(newTree, sym)
                  denot.info = newInfo.derivedExprType(newResType)
              updateInfo(sym, newInfo)
            case tp =>
              // println(s"${tp} <- ${sym.show}")
          end if
          newTree

        case tree @ ValDef(name, tpt, rhs) =>
          val sym = tree.symbol
          val newTree = super.transform(tree).asInstanceOf[ValDef]
          if sym.exists && !sym.is(Param) && !sym.is(Module) then
            val forcedRes = tpt.asInstanceOf[TypeTree]
            if forcedRes.isInferred then
              val newInfo = sym.info.dropAllKill
              val updatedInfo = new LazyType:
                def complete(denot: SymDenotation)(using Context): Unit =
                  assert(ctx.phase == thisPhase.next, i"$sym")
                  denot.info = newInfo
                  val newResType = recheckDef(newTree, sym)
                  denot.info = newResType

              updateInfo(sym, updatedInfo)
          end if
          newTree

        case tree @ Ident(_) =>
          super.transform(tree.withType(tree.tpe.boxedToTermRef))
        case _ =>
          super.transform(tree)
      end transformedTree
      transformedTree.setCCType(tree.ccType)
      transformedTree
    end transform

  class FXSetupTransformer(checker: CheckEffects.FXCheckerAPI) extends TreeMapWithPreciseStatContexts:
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