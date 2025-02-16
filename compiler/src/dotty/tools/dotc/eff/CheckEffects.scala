package dotty.tools
package dotc
package eff

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*, Denotations.*
import ast.tpd, tpd.*
import transform.{PreRecheck, Recheck}
import annotation.tailrec
import cc.isRetainsLike
import Recheck.isUpdatedAfter

object CheckEffects:
  val name: String = "eff"
  val description: String = "effect checking"

  /**
   * TODO checkWellFormed
   * Needs to be capturing type (I believe this already enforces reference type)
   * Must only refer to argument - this should already be checked at function decl.
   * Would be nice to enforce annotation to only occur in function return type, but idk how.
   */
  // def checkWellformed(parent: Tree, ann: Tree)(using Context): Unit =
  //   ann.killedElems.foreach: ref =>
  //     if !ref.symbol.isRetainsLike then ()
  //       // report.error(i"Killed variable '$ref' may not be a capability.", ref.srcPos)

  extension (tree: Tree)
    /**
     * Given a annotation, finds its killedElems from its tree (annot.tree).
     */
    def killedElems(using Context): List[Tree] = tree match
      case Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil) =>
        elems
      case _ =>
        // report.error(i"$tree's killed elements are not well-formed!")
        Nil

  extension (sym: Symbol)
    def isKillEff(using Context): Boolean =
      sym.name.toString == "kill"

end CheckEffects

class CheckEffects extends Recheck, SymTransformer:
  thisPhase =>

  override def phaseName: String = CheckEffects.name

  override def description: String = CheckEffects.description

  override def isRunnable(using Context) = true

  /**
   * Problem: I want to use symbol denotations from capture checking, but
   * transformSym in Recheck.scala reset all symdenotations to their state before setupCC
   * after capture checking finishes.
   *
   * Solution: I change transformSym in checkCaptures to do nothing and put the setup phase
   * of effect checking as setupCC.
   * Problem is I have to modify capture checker + I may want to have a setup phase for effect checking
   * in the future anyways.
   *
   * Ideal solution is to have a separate setup phase before effect checking which keeps cc sym info,
   * so that I don't have to modify recheck.scala/capture checker.
   */
  override def preRecheckPhase: PreRecheck =
    val setupCCPhase = this.prev.prev
    if setupCCPhase.phaseName == "setupCC" then
      setupCCPhase.asInstanceOf[PreRecheck]
    else
      assert(false, "Effect checking must be directly after capture checking phase!")

  def newRechecker()(using Context): Rechecker = EffectChecker(ctx)

  /**
   * Problem - how to deal with aliasing
   * Possible solution? Since we enforce only capabilities are tracked, any alias of the
   * capabilitiy will have the capability in its capture set, and maybe can use subcapturing?.
   * e.g. val f = new File() is cap, then f2 = f would be File^{f}, so maybe we can check if
   * capture set of f2 = {f} <: {f} = singleton capture set of f (not capture set of f since that is {cap}).
   */
  class EffectChecker(ictx: Context) extends Rechecker(ictx):
  /**
    * We will need some sort of killed variables data structures:
    * - Have killed syms set
    *     Problem: Can't have as extra parameter + Mutable global state
    *     But being global is maybe good? Should not be like environment.
    * - Extend context with new field for killed syms?
    * - Change sym denotation when it is killed?
    */
    private val killedSyms = util.HashSet[Symbol]()

    override def recheckIdent(tree: tpd.Ident, pt: Type)(using Context): Type = {
      if killedSyms.contains(tree.symbol) then
        report.error(i"Use of '${tree.symbol}' is prohibited after kill.", tree.srcPos)
      super.recheckIdent(tree, pt)
    }

    override def recheckValDef(tree: tpd.ValDef, sym: Symbol)(using Context): Type = {
      // if redeclaration of killed val, then have to remove from killedSyms
      // nvm redeclarated val is different sym! so nice!
      // println(s"${sym.denot.info} <- ${sym.show}")
      super.recheckValDef(tree, sym)
    }

    override def recheckDefDef(tree: tpd.DefDef, sym: Symbol)(using Context): Type = {
      // println(s"${tree.name.show} = ${tree.tpt}")
      super.recheckDefDef(tree, sym)
    }

    override def recheckApply(tree: tpd.Apply, pt: Type)(using Context): Type = {
      val appType = super.recheckApply(tree, pt)
      appType match
        case EffectType(parent, refs) => refs.foreach(killedSyms += _.symbol)
        case _ => ()
      appType
    }

    override def recheckBlock(tree: tpd.Block, pt: Type)(using Context): Type = {
      super.recheckBlock(tree, pt)
    }

    // override def recheckStats(stats: List[tpd.Tree])(using Context): Unit = {
    //   @tailrec def traverse(stats: List[Tree])(using Context): Unit = stats match
    //     case (imp: Import) :: rest =>
    //       traverse(rest)(using ctx.importContext(imp, imp.symbol))
    //     case stat :: rest =>
    //       recheck(stat)
    //       traverse(rest)
    //     case _ =>
    //   traverse(stats)
    // }



