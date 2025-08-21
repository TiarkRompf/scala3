package scala
package typestate

import annotation.{experimental, StaticAnnotation, retainsArg}

/**
  * Annotation that indicates killed set of references.
  * @param xs killed set of references
  */
@experimental
class kill(xs: Any*) extends annotation.StaticAnnotation

/**
  * Function self reference.
  */
@experimental
object FUN

/**
  * Implicit Dependent Pair - the type of b can depend on term a
  */
@experimental
trait Sigma:
  type A
  type B
  val a: A
  val b: B

@experimental
type `Pair`[A1, B1] = Sigma { type A = A1; type B = B1 }

@experimental
type IBox[T] = Sigma { type A = Unit; type B = T }

@experimental
object Sigma:
  def apply[A1, B1](a1: A1, b1: B1): `Pair`[A1, B1] =
    new Sigma:
      type A = A1
      type B = B1
      val a = a1
      val b = b1

// /**
//   * Annotation which stops ANF transformation if annotated on
//   * a transformation-triggering type.
//   */
// @experimental
// class StopTransform extends annotation.StaticAnnotation

// /**
//   * Singleton tuple which returns its inhabitant implictly.
//   *
//   * @param obj inhabitant to be returned implicitly
//   */
// @experimental
// class IBox[T](val obj: T)
