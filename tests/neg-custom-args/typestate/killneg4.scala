package typestate

import language.experimental.captureChecking
import caps.*
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation
object FUN

class myCap:
  type pathCap

object KillNeg4:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def subFree() =
    val l: myCap^ = new myCap
    val y: myCap^ = new myCap
    def func(f: () => Unit @kill(l)) = ???
    val g = () =>
      kmyCap(y)
    func(g) // error
    ()