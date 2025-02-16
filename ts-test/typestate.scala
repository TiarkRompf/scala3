package typestate

import language.experimental.captureChecking
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation

// class ClosedFile(val name: String)
// class OpenFile(val name: String)

// object File:
//   def open(f: ClosedFile^): (OpenFile^) @kill(f) = // note kill is well-formed iff it has at least one argument
//     new OpenFile(f.name)

// class File(val name: String):
//   class isClosed()
//   class isOpen()

object Main:
  // import File.*
  // def t1() =
  //   val f1 = new ClosedFile("foo.txt")
  //   val f2 = open(f1)
  //   val bad = f1
  //   val x = 32
  def free(y: Int): Unit @kill(y) = ()

  def t2() = {
    val x = 42
    val y = 32
    // // free(x)
    {
      val a = x
      val b = y
    }
  }


