package dotty.tools
package dotc
package eff

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*, Denotations.*
import ast.tpd, tpd.*
import transform.{PreRecheck, Recheck}
import annotation.{tailrec, threadUnsafe}
import cc.*
import Recheck.*
import NamerOps.linkConstructorParams

object CheckEffects:
  val name: String = "eff"
  val description: String = "effect checking"

  extension (tree: Tree)
    /**
     * Given a annotation, finds its killedElems from its tree (annot.tree).
     */
    def killedElems(using Context): List[Tree] = tree match
      case Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil) =>
        elems
      case _ =>
        Nil

  var killAnnot: Option[Symbol] = None // very hacky solution for now

  var useAnnot: Option[Symbol] = None

  extension (sym: Symbol)
    def isKill(using Context): Boolean =
      killAnnot match
        case None =>
          killAnnot = Some(requiredClass("typestate.kill"))
          sym == requiredClass("typestate.kill")
        case Some(annotSym) => sym == annotSym

    def isUse(using Context): Boolean =
      useAnnot match
        case None =>
          useAnnot = Some(requiredClass("typestate.use"))
          sym == requiredClass("typestate.use")
        case Some(annotSym) => sym == annotSym

    def isEff(using Context): Boolean =
      sym.isKill || sym.isUse

  extension (tp: Type)
    def dropKill(using Context): Type = // maybe recursively drop?
      tp match
        case EffectType(parent, killSet) => parent
        case _ => tp

    def getKilled(using Context): List[Symbol] =
      tp match
        case EffectType(_, killSet) => killSet.map(_.symbol)
        case AppliedType(tycon, args) => // it seems like AppliedTypes have more "priority" then AnnotatedTypes
          tycon.getKilled ++ args.map(_.getKilled).flatten
        case _ => Nil

  /**
   * Checks that the variables inside a kill annotation are well-formed
   * Well-formedness conditions:
   * 1. Must be non-empty
   * 2. Must be a capability - capturing type/retaining type/extends Capability or something in caps
   * 3. Must only refer to argument of function - need to check this!
   * 4. Kill annotation can only appear at function return type - idk how to check this
   * 5. No duplicates allowed e.g. no kill(f, f).
   */
  def checkWellformed(annot: Tree)(using Context): Unit =
    val killedElems = annot.killedElems
    if killedElems.isEmpty then
      report.error(i"Kill set of $annot may be empty.", annot.srcPos)
    else
      val killSet = util.HashSet[Symbol]()
      for ref <- killedElems do
        val refSym = ref.symbol
        if !killSet.add(refSym) then
          report.error(i"Kill set of $annot has a duplicate ref $ref", annot.srcPos)
        ref.tpe.widen match
          case RetainingType(_) => ()
          case CapturingType(_) => () // I don't think these should ever exist during typer phase
          case _ =>
            // println(ref.symbol.denot.info)
            // TODO figure out how to check if class extends capability or a
            // TODO find more capabilities identifying info for extends capability

  object EffectType:
    def unapply(tp: Type)(using Context): Option[(Type, List[Tree])] =
      tp match
        case AnnotatedType(parent, annot) if annot.symbol.isKill =>
          Some(parent, annot.tree.killedElems)
        case _ => None
end CheckEffects

/*
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
   * Ideal solution is probably to have a separate setup phase before effect checking which keeps cc sym info,
   * so that I don't have to modify recheck.scala/capture checker.
   */
  override def preRecheckPhase: PreRecheck =
    val setupCCPhase = this.prev.prev
    if setupCCPhase.phaseName == "setupCC" then
      setupCCPhase.asInstanceOf[PreRecheck]
    else
      assert(false, "Effect checking must be directly after capture checking phase!")

  def newRechecker()(using Context): Rechecker = EffectChecker(ctx)

  class EffectChecker(ictx: Context) extends Rechecker(ictx):
    import CheckEffects.*
  /**
    * We will need some sort of killed variables data structures:
    * - Have killed syms set
    *     Problem: Can't have as extra parameter + Mutable global state
    *     But being global is maybe good? Should not be like environment.
    * - Extend context with new field for killed syms?
    * - Change sym denotation when it is killed?
    *
    * Future changes to killedSyms - make it have an owner and properly scope
    */
    private val killedSyms = util.HashSet[Symbol]()

    override def recheckIdent(tree: tpd.Ident, pt: Type)(using Context): Type = {
      // println(tree.tpe.widen)
      if killedSyms.contains(tree.symbol) then
        report.error(i"Use of '${tree.symbol}' is prohibited after kill.", tree.srcPos)
      super.recheckIdent(tree, pt)
    }

    /**
     * Setup phase before effect checking (and capture checking)
     * We only want in kill effect annotation in this position
     * A => B @kill
     * This should be checked in typer.
     *
     * But sometimes in inference @kill may naturally arise in val pos
     * for example
     *
     * def open() @kill = ...
     *
     * val o = open() // o will have inferred type with @kill
     *
     * we want to strip those?
     */
    override def recheckValDef(tree: tpd.ValDef, sym: Symbol)(using Context): Type = {
      super.recheckValDef(tree, sym)
    }

    override def recheckDefDef(tree: tpd.DefDef, sym: Symbol)(using Context): Type = {
      println(tree.rhs)
      inContext(linkConstructorParams(sym).withOwner(sym)):
        val argSyms = tree.termParamss.flatten.map(_.symbol)
        val resType = recheck(tree.tpt)
        // println(s"${tree.tpe.widen} <- ${sym.name.show}")

        // if (sym.name.toString == "HO") then
        //   tree.termParamss.flatten.foreach(param => println(param.asInstanceOf)) // valdef with no rhs

        if tree.rhs.isEmpty || sym.isInlineMethod || sym.isEffectivelyErased
        then resType
        else
          val rhsType = recheck(tree.rhs, resType)
          if (sym.isRealMethod) then
            tree.tpt match
              case _: InferredTypeTree =>
                for arg <- argSyms do
                  if killedSyms.contains(arg) then
                    report.error(i"Parameter ${arg.name} is killed in ${sym.name} but ${sym.name} has no kill annotation!",
                    tree.srcPos)
              case _ =>
                val killSet = resType match
                  case EffectType(_, elems) => elems.map(_.symbol).toSet
                  case _ => Nil.toSet

                for arg <- argSyms do
                  if killedSyms.contains(arg) then
                    if killSet.isEmpty then
                      report.error(i"Parameter ${arg.name} is killed in ${sym.name} but ${sym.name} has no kill annotation!",
                      tree.srcPos)
                    else if !killSet.contains(arg) then
                      report.error(i"Kill set of ${sym.name} does not contain killed argument ${arg.name}",
                      tree.srcPos)

          // if (sym.name.toString == "$anonfun") then
          //   println(tree.tpt.asInstanceOf[TypeTree].isInferred)
          //   println(tree.tpt.tpe)
            // println(tree.tpe.widen)
          rhsType
    }

    override def recheckApply(tree: tpd.Apply, pt: Type)(using Context): Type = {
      val appType = super.recheckApply(tree, pt)
      appType match
        case EffectType(parent, refs) =>
          refs.foreach(killedSyms += _.symbol)
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

    override def recheckTypeTree(tree: tpd.TypeTree)(using Context): Type =
      tree.nuType

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      super.checkUnit(unit)
*/

