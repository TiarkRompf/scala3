package scala
package typestate

import annotation.{experimental, StaticAnnotation, retainsCap}
// retainsCap is internal annotation for ^

/**
 * Annotation that indicates killed set of references.
 * @param xs killed set of references
 */
@experimental
final class kill(xs: Any*) extends annotation.StaticAnnotation

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

/**
 * def ..(): B1 ?<= A1
 * returns B1 implicitly and A1 explicitly
 */
@experimental
infix type ?<=[B1, A1] = (Sigma { type A = A1; type B = B1 }) @retainsCap()

/**
 * @param a1 - first value of Sigma (returned explicitly)
 * @param a2 - second value of Sigma (returned implicitly)
 */
@experimental
def Sigma[A1, B1](a1: A1, b1: B1): Sigma { type A = A1; type B = B1 } =
  new Sigma:
    type A = A1
    type B = B1
    val a: A1 = a1
    val b: B1 = b1

/**
 * Function that takes c and kills it.
 *
 */
@experimental
infix type =!>[T, U] = (c: T) => (U) @kill(c)

/**
 * Function that takes c implicitly and kills it.
 */
@experimental
infix type ?=!>[T, U] = (c: T) ?=> (U) @kill(c)


/**
  * State transition alias, abstracts common pattern for
  * transitioning between path-dependent types signifying states.
  *
  * Function that takes in c: S1^ implicitly and
  * returns S2^ implicitly, killing c.
  *
  */
@experimental
infix type ?=!>?[S1, S2] = (c: S1 @retainsCap()) ?=> ( ( Sigma {type A = Unit; type B = S2 @retainsCap()}) @retainsCap()) @kill(c)