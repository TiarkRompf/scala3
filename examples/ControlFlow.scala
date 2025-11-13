import language.experimental.captureChecking
import caps.*
import typestate.*

def move[T]: T ?=!>? T =
  Sigma((), summon[T].asInstanceOf[T])

def ifDiff[T, B1, B2](using c: T^)(cond: => Boolean)[A1](tbranch: (T^) ?=!> ((B1^) ?<= A1))
    (ebranch: (T^) ?=!> ((B2^) ?<= A1)): ((Either[B1, B2]^) ?<= A1) @kill(c) =
  if cond then
    val sigma = tbranch : ((B1^) ?<= A1)
    val t: sigma.a.type = sigma.a
    val b1 = sigma.b
    Sigma(t, Left(b1.asInstanceOf[B1]))
  else
    val sigma = ebranch : ((B2^) ?<= A1)
    val t: sigma.a.type = sigma.a
    val b2 = sigma.b
    Sigma(t, Right(b2.asInstanceOf[B2]))

def matchSame[B1, B2, T](using c: Either[B1, B2]^)[U](left: (B1^) ?=!> ((T^) ?<= U))
    (right: (B2^) ?=!> ((T^) ?<= U)): ((T^) ?<= U) @kill(c) =
  c match
    case Left(b1) => left(using b1.asInstanceOf[B1^])
    case Right(b2) => right(using b2.asInstanceOf[B2^])

def loop[T](using c: T^)(cond: => Boolean)(body: T ?=!>? T): ((T^) ?<= Unit) @kill(c) =
  if cond then
    body
    loop[T](cond)(body)
  else move[T]

def whileLeft[T, U](using c: T^)(body: T ?=!>? Either[T, U]): ((U^) ?<= Unit) @kill(c) =
  body(using c)
  matchSame[T, U, U] {
    whileLeft[T, U](body)
  } { move[U] }

// def matchC[A, B, T](using c: Either[A, B]^)[U](left: (A^) ?=!> ((T^) ?<= U))(right: (B^) ?=!> ((T^) ?<= U)): ((T^) ?<= U) @kill(c) =
//   c match
//     case Left(b1) =>
//       left(using b1.asInstanceOf[A^])
//     case Right(b2) =>
//       right(using b2.asInstanceOf[B^])

// def ifOne[T](using c: T^)(cond: => Boolean)[U](tbranch: (T^) ?=!> ((T^) ?<= U))
//   (ebranch: => U): ((T^) ?<= U) @kill(c) =
//   if cond then
//     tbranch
//   else
//     ebranch // will not work since implicit found is T^{c} not T^
