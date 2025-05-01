package typestate

import language.experimental.captureChecking
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation
object FUN

class myCap:
  type pathCap

object KillNeg1:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def basic(jj: myCap^) =
    kmyCap(jj)
    val k = jj // error

  def basicAlias() =
    val a: myCap^ = new myCap
    val b = a
    val c = a
    val d = a
    kmyCap(d)
    val bad = a // error
    val bad2 = c // error

  def basicAlias2() =
    val a: myCap^ = new myCap
    val b = a
    val c = a
    val d = a
    kmyCap(a)
    val bad = d // error
    val bad2 = b // error

  def pure(alf: myCap^): Unit = ()

  def HOPure(func: myCap^ => Unit): Unit = ()

  def HOKillM(func: (x: myCap^, y: myCap^, z: myCap^) => Unit @kill(x, z)): Unit = ()

  def kmyCap3(a: myCap^, b: myCap^, c: myCap^): Unit @kill(a, b) = ()

  def useHO1(): Unit =
    HOPure(kmyCap) // error

  def inferTest(l: myCap^, j: myCap^, k: myCap^) =
    kmyCap(k)
    kmyCap(l)
    kmyCap(j)

  def useHO2(): Unit =
    HOKillM(inferTest) // error
    HOKillM(kmyCap3) // error

  def testVal1() =
    val g = (f: myCap^) =>
      kmyCap(f)
      val j = 23984
    val someCap: myCap^ = new myCap
    g(someCap)
    val badUse = someCap // error

  def testVal2() =
    val g2: myCap^ => Unit = f => // error
      kmyCap(f)
      val j = 23882
    ()

