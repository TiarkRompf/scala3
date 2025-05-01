package typestate

import language.experimental.captureChecking
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation
object FUN

class myCap:
  type pathCap

object KillSubPos:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def s1():  (myCap^ => Unit) => ((y: myCap^) => Unit @kill(y)) =
    0.asInstanceOf[((j: myCap^) => Unit @kill(j)) => (myCap^ => Unit)]

  def s2(): ((x: myCap^, y: myCap^, z: myCap^) => Unit @kill(z, x)) =
    0.asInstanceOf[(a: myCap^, b: myCap^, c: myCap^) => Unit @kill(a, c)]

  def s3(): (x: myCap^) => ((y: myCap^) => Unit @kill(y)) @kill(x) =
    def inner(r: myCap^) = kmyCap(r)
    (x: myCap^) =>
      kmyCap(x)
      inner

  def loopCPS(f: myCap^)
  (body: ((fK: myCap^) => Unit @kill(fK)) -> (f2: myCap^) => Unit @kill(f2)): Unit @kill(f) =
    val K: (f3: myCap^) -> Unit @kill(f3) =
      f => loopCPS(f)(body)
    body(K)(f)

  def testLoopCPS(b: String): Unit =
    val g: myCap^ = new myCap
    loopCPS(g) { (K) => (f) =>
      ()
    }
