package dotty.tools
package dotc
package typer

import core.*
import Symbols.*, Types.*, Contexts.*, Flags.*, Names.*, NameOps.*, NameKinds.*
import StdNames.*, Denotations.*, Phases.*, SymDenotations.*

/**
 * Ordinary dependent pair stuff
 */
var dependentPair: ClassSymbol | Null = null

def getDependentPair(using Context): ClassSymbol =
  dependentPair match
    case null =>
      dependentPair = requiredClass("typestate.DependentPair")
      dependentPair.uncheckedNN
    case sym => sym

def isDependentPair(tpe: Type)(using Context) =
  tpe.typeSymbol == getDependentPair
  // tpe.typeSymbol.toString == dependentPair.toString

object DependentPair:
  def unapply(tp: Type)(using Context): Option[(Type, Type)] = tp match
    case AppliedType(tpe, args) if isDependentPair(tpe) =>
      Some(args(0), args(1))
    case _ =>
      None

/**
 * Sigma type
 */
var Sigma: ClassSymbol | Null = null

def getSigma(using Context): ClassSymbol =
  Sigma match
    case null =>
      Sigma = requiredClass("typestate.Sigma")
      Sigma.uncheckedNN
    case sym => sym

def isSigma(tpe: Type)(using Context) =
  tpe.typeSymbol == getSigma

object SigmaPair:
  def unapply(tp: Type)(using Context): Option[Type] = tp match
    case tpe if isSigma(tp) => Some(tp)
    case _ => None

/**
 * Hack for substituting in recursive types.
 *
 * Consider pt = Sigma { type A = File; type B = a.IsOpen }
 * Then we want to summon a type of a.IsOpen, where
 * a is the expected tree. To do this,
 * we need to substitute the TermRef of the tree for a.
 *
 * This method does so -
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

