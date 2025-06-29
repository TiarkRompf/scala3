package typestate

import language.experimental.captureChecking
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation
object FUN

class File(val name: String):
  type isClosed
  type isOpen

object File:
  def open(f: File, c: f.isClosed^): (f.isOpen^) @kill(c) =
    1.asInstanceOf[f.isOpen]

  def close(f: File, c: f.isOpen^): (f.isClosed^) @kill(c) =
    0.asInstanceOf[f.isClosed]

  def read(f: File, c: f.isOpen^): String = "TODO"

  def write(f: File, s: String, c: f.isOpen^): Unit = ()

  def withFile[T](name: String)(body: (f: File) => (c: f.isClosed^) => ((T, f.isClosed^) @kill(c))): T =
    val file = new File(name):
      type isClosed = Int
      type isOpen = Int
    body(file)(0)._1

object FileMain2:
  import File.*
  def test1() =
    withFile("a.txt") { (f) => (c) =>
      val o = open(f, c)
      ((), c) // error
    }




