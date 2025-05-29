package typestate

import language.experimental.captureChecking
import caps.*
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation
object FUN

class myCap:
  type pathCap

object KillNeg3:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def killFree() =
    val k: myCap^ = new myCap

    def inner1() =
      kmyCap(k)

    inner1()
    inner1() // error

  def selfKill(): Unit @kill(FUN) = ()

  def testSelfKill(): Unit =
    selfKill()
    selfKill() // error

  def whileTest(b: Boolean): Unit =
    val l: myCap^ = new myCap
    while (b) do
      kmyCap(l) // error

  def whileTest2(b: Boolean): Unit =
    val l2: myCap^ = new myCap
    val k = l2
    while (b) do
      def somethingBad() = // error
        kmyCap(k)
      somethingBad()

