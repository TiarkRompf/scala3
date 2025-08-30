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
            tycon.symbol == defn.TSPair ||
            tycon.underlying.typeSymbol == defn.Sigma
          case _ => false
      }

    def derivedSigma2Type(tp1: Type, tp2: Type)(using Context): Type = tp match
      case tp @ Sigma2Type(fst, scd) =>
        if (tp1 eq fst) && (tp2 eq scd) then tp
        else Sigma2Type(tp1, tp2)

  // Generic Sigma unwrapper
  object SigmaType:
    def unapply(tp: Type)(using Context): Option[Type] = tp match
      case tpe if isSigma(tp) => Some(tp)
      case _ => None

  // Sigma { type A = ... ; type B = ... }
  object Sigma2Type:
    def unapply(tp: Type)(using Context): Option[(Type, Type)] = tp match
      case RefinedType(RefinedType(tref, A, TypeAlias(tp1)),
          B, TypeAlias(tp2)) if isSigma(tref) =>
            Some(tp1, tp2)
      case _ => None

    def apply(tp1: Type, tp2: Type)(using Context): Type =
      RefinedType(
        RefinedType(defn.Sigma.typeRef, A, TypeAlias(tp1)),
        B, TypeAlias(tp2)
      )

  // Sigma { type A = ... }
  object SigmaAType // TODO

  // Sigma { type B = ... }
  object SigmaBType // TODO

  // Sigma
  object Sigma0Type // TODO

  /**
   * Hack for substituting in recursive types.
   *
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

