package typestate

import language.experimental.captureChecking
import caps.Capability
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation

class File(name: String):
  class isClosed() // extends Capability
  class isOpen() // extends Capability
  // type isClosed
  // type isOpen

object File:
  def mkFile(name: String) = // cannot annotate type
    val file = new File(name)
    (file, file.isClosed())

  def open(f: File, c: f.isClosed^): (f.isOpen^) @kill(c) =
    new f.isOpen()

  def withFile(name: String)(body: (f: File) => (c: f.isClosed^) => (f.isClosed^{c})) =
    val file = new File(name)
    val fileCap: file.isClosed^ = new file.isClosed()
    body(file)(fileCap)

object Main:
  import File.*
  // def test1: Unit =
  //   withFile("a.txt") { (f1, c1) =>
  //     withFile("b.txt") { (f2, c2) =>
  //       c2
  //     }
  //     c1
  //   }
