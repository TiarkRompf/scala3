package typestate

import language.experimental.captureChecking
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation
object FUN

class myCap:
  type pathCap

object KillPos2:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def escaping1(): () => Unit @kill(FUN) =
    val j1 = {
      val k: myCap^ = new myCap
      val foo = () =>
        kmyCap(k)
        ()
      foo
    }
    j1
