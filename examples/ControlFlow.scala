import language.experimental.captureChecking
import caps.*
import typestate.*

class File(val path: String):
  type IsClosed
  type IsOpen

object File:
  def apply(path: String): Sigma { type A = File; type B = a.IsClosed^ } =
    val f = new File(path):
      type IsClosed = Unit
      type IsOpen = Unit
    new Sigma:
      type A = File
      type B = a.IsClosed^
      val a: f.type = f
      val b: f.IsClosed^ = ()

extension (f: File)
  def open(): f.IsClosed ?=!>? f.IsOpen =
    Sigma((), ().asInstanceOf[f.IsOpen])

  def close(): f.IsOpen ?=!>? f.IsClosed =
    Sigma((), ().asInstanceOf[f.IsClosed])

  def read(): f.IsOpen^ ?=> String = ""

  def write(line: String): f.IsOpen^ ?=> Unit = ()

def withFile[T](path: String)(body: (f: File) => (f.IsClosed^) =!> (f.IsClosed^) ?<= T): T =
  val f = new File(path):
    type IsClosed = Unit
    type IsOpen = Unit
  ((body(f)(())) : ((f.IsClosed^) ?<= T)).a

def move[T]: T ?=!>? T =
  Sigma((), summon[T].asInstanceOf[T])

def ifDiff[T, B1, B2](using c: T^)(cond: => Boolean)[A1](tbranch: (T^) ?=!> ((B1^) ?<= A1))
    (ebranch: (T^) ?=!> ((B2^) ?<= A1)): ((Either[B1, B2]^) ?<= A1) @kill(c) =
  if cond then
    val a1 = tbranch(using c) // returns B1^ implicitly and A1 explicitly
    Sigma(a1, Left(summon[B1].asInstanceOf[B1]))
  else
    val a2 = ebranch(using c) // returns B1^ implicitly and A1 explicitly
    Sigma(a2, Right(summon[B2].asInstanceOf[B2]))

def matchSame[B1, B2, T](using c: Either[B1, B2]^)[U](left: (B1^) ?=!> ((T^) ?<= U))
    (right: (B2^) ?=!> ((T^) ?<= U)): ((T^) ?<= U) @kill(c) =
  c match
    case Left(b1) => left(using b1.asInstanceOf[B1^])
    case Right(b2) => right(using b2.asInstanceOf[B2^])

def loop[T](using c: T^)(cond: => Boolean)(body: T ?=!>? T): ((T^) ?<= Unit) @kill(c) =
  if cond then
    body(using c)
    loop[T](cond)(body)
  else move[T]

def whileLeft[T, U](using c: T^)(body: T ?=!>? Either[T, U]): ((U^) ?<= Unit) @kill(c) =
  body(using c)
  matchSame[T, U, U] {
    whileLeft[T, U](body)
  } { move[U] }

def ifOne[T](using c: T^)(cond: => Boolean)[U](tbranch: (T^) ?=!> ((T^) ?<= U))
  (ebranch: => U): ((T^) ?<= U) @kill(c) =
  if cond then
    tbranch
  else
    val res = ebranch
    move[T]
    res

object Main:
  import File.*
  def one() =
    withFile("a.txt") { f => c =>
      f.open()(using c)
      f.close()
      ifDiff[f.IsClosed, f.IsClosed, f.IsOpen] (true) {
        f.open()
        f.close()
        val k = 202
      } {
        f.open()
      }
      val k = 201
      matchSame[f.IsClosed, f.IsOpen, f.IsOpen] {
        val k = 2397
        f.open()
      } {
        f.close()
        f.open()
      }
      f.close()
    }

  def two(b: Boolean) =
    val f = File("a.txt")
    loop[f.IsClosed] (b) {
      f.open()
      f.close()
      val k = 120
    }
    f.open()
    f.close()
    ()

