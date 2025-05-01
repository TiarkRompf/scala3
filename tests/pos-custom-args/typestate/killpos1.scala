package typestate

import language.experimental.captureChecking
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation
object FUN

class myCap:
  type pathCap

object Kill2:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def inferTest(l: myCap^, j: myCap^, k: myCap^) =
    kmyCap(k)
    kmyCap(l)

  def callInfer(a: myCap^, b: myCap^): Unit @kill(a, b) =
    inferTest(a, b, new myCap)

  def pure(alf: myCap^): Unit = ()

  def HOKill(func: (x: myCap^) => Unit @kill(x)): Unit = ()

  def HOPure(func: myCap^ => Unit): Unit = ()

  def HOKillM(func: (x: myCap^, y: myCap^, z: myCap^) => Unit @kill(x, z)): Unit = ()

  def kmyCap3(a: myCap^, b: myCap^, c: myCap^): Unit @kill(a, c) = ()

  def useHO(): Unit =
    HOKillM(inferTest)
    HOKill(pure)
    HOKill(kmyCap)
    HOKill {
      val g = (f: myCap^) => ()
      val k = g
      k
    }

  def testKillValue() =
    kmyCap(new myCap)

  def scopingTest(): Unit =
    val f: myCap^ = new myCap
    {
      val f: myCap^ = new myCap
      kmyCap(f)
    }
    val k = f
