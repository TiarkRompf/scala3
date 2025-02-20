package typestate

import language.experimental.captureChecking
import caps.Capability
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation

class ClosedFile(val name: String) // extends Capability - whenever you create a new ClosedFile - it will have ^{cap}
class OpenFile(val name: String) // extends Capability

object File:
  def open(f: ClosedFile^): (OpenFile^{f}) @kill(f) =
    new OpenFile(f.name)

  def close(f: OpenFile^): (ClosedFile^{f}) @kill(f) =
    new ClosedFile(f.name)

  def read(f: OpenFile^): String = "read"

  def withFile[T](name: String)(body: (f: ClosedFile^) => (T, ClosedFile^{f})): Unit = {
    body(new ClosedFile(name))
  }

  // def withFileU(name: String)(body: (f: ClosedFile^) => (ClosedFile^{f})): Unit = {
  //   body(new ClosedFile(name))
  // }

object Main:
  import File.*

  // def testalias(fil: ClosedFile): ClosedFile^{fil} =
  //   val fil2 = fil
  //   val fil3 = fil2
  //   val fil4 = new ClosedFile("foo")
  //   fil4

  // def test1(fil: ClosedFile^): Unit = // should fail
  //   val x = 43
  //   open(fil)
  //   val f2 = fil

  // def test2(): Unit = // should fail
  //   withFile("a.txt") { file1 =>
  //     withFile("b.txt") { file2 =>
  //       ((), file1)
  //     }
  //     ((), file1)
  //   }

  def test3(): Unit =
    withFile("a.txt") { file =>
      val f2 = open(file)
      val msg = read(f2)
      val f3 = close(f2)
      (msg, f3)
    }

  /** recheckApply && recheckDefDef tests */
  // def argHO(file: ClosedFile^)(func: (f: ClosedFile^) => (OpenFile^{f}) @kill(f) ): Unit =
  //   val tmp = func(file)
  //   ()

  // def retHO(): ((f: ClosedFile^) => (OpenFile^{f}) @kill(f)) =
  //   open

  // def eint(file: ClosedFile^): Int @kill(file) =
  //   val lol = open(file)
  //   5

  // def propArg(file: ClosedFile^)(j: Int, y: Int): Unit @kill(file) =
  //   val x = 10 + eint(file)
  //   ()

  // def HO(y: Int, f: (x: Int) => Unit @kill(x)): Unit @kill(y) =
  //   f(y)
  //   ()

  // def pure(x: Int): Unit = ()

  // // def notPure(x: Int): Int @kill(x) = ()

  // def useHO(x: Int): Unit @kill(x) =
  //   // HO(x, pure) // needs to reject this
  //   // HO(IntKill) // needs to accept this


