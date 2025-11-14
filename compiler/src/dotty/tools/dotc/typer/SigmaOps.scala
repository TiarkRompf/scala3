package dotty.tools
package dotc
package typer

import core.*
import Symbols.*, Types.*, Contexts.*, Flags.*, Names.*, NameOps.*, NameKinds.*
import StdNames.*, Denotations.*, Phases.*, SymDenotations.*
import ast.*

object SigmaOps:
  private val A = typeName("A")
  private val B = typeName("B")

  extension (tp: Type)
    def isSigma(using Context) =
      tp.dealias.typeSymbol == defn.Sigma
      || {
        tp.dealias match
          case AppliedType(tycon: TypeRef, _) => // hack for type Pair
            tycon.symbol == defn.ImplicitRet ||
            tycon.underlying.typeSymbol == defn.Sigma
          case _ => false
      }

    def derivedSigma2Type(tp1: Type, tp2: Type)(using Context): Type = tp match
      case tp @ Sigma2Type(fst, scd) =>
        if (tp1 eq fst) && (tp2 eq scd) then tp
        else Sigma2Type(tp1, tp2)

    def getFstAlias(using Context): Option[TypeAlias] = tp match
      case RefinedType(RefinedType(tref, A, fstAlias: TypeAlias), B, _: TypeAlias) if tref.isSigma =>
        Some(fstAlias)
      case _ => None

    def getScdAlias(using Context): Option[TypeAlias] = tp match
      case RefinedType(RefinedType(tref, A, _: TypeAlias), B, scdAlias: TypeAlias) if tref.isSigma =>
        Some(scdAlias)
      case _ => None

    /**
     * Given tp, mark each type member of a Sigma Type as
     * a sigma type member. This is useful since we
     * need to treat these as covariant instead of invariant.
     */
    def markSigmaMembers(using Context) =
      val marker = new TypeTraverser:
        def traverse(tp: Type): Unit = tp match
          case tp @ Sigma2Type(fst, scd) =>
            traverse(fst)
            traverse(scd)
            tp.getFstAlias.foreach(_.isSigmaTypeMember = true)
            tp.getScdAlias.foreach(_.isSigmaTypeMember = true)
          case _ => traverseChildren(tp)
      marker.traverse(tp)

  // Generic Sigma unwrapper
  object SigmaType:
    def unapply(tp: Type)(using Context): Option[Type] = tp match
      case tpe if isSigma(tp) => Some(tp)
      case _ => None

  // Sigma { type A = ... ; type B = ... }
  object Sigma2Type:
    def unapply(tp: Type)(using Context): Option[(Type, Type)] = tp match
      case RefinedType(RefinedType(tref, A, TypeAlias(tp1)), B, TypeAlias(tp2)) if tref.isSigma =>
        Some(tp1, tp2)
      case _ => None

    def apply(tp1: Type, tp2: Type)(using Context): Type =
      RefinedType(
        RefinedType(defn.Sigma.typeRef, A, TypeAlias(tp1)),
        B, TypeAlias(tp2)
      )

  // Sigma { type A = ... }
  object SigmaAType

  // Sigma { type B = ... }
  object SigmaBType

  // Sigma
  object Sigma0Type

  /**
   * Consider pt = Sigma { type A = File; type B = a.IsOpen }
   * Then we want to summon a type of a.IsOpen, where
   * a is the expected tree. To do this,
   * we need to substitute the TermRef of the tree for a.
   *
   * @param from is a.IsOpen
   * @param to is TermRef of tree to be substituted for a
   * @param selfRef is `a`, e.g. TermRef(RecThis(...), a)
   */
  def substSigmaMember(from: Type, to: Type, selfRef: Type)(using Context): Type =
    val sigmaSubstMap = new TypeMap:
      def apply(tp: Type): Type =
        if tp == selfRef then to
        else mapOver(tp)
    sigmaSubstMap(from)
end SigmaOps

