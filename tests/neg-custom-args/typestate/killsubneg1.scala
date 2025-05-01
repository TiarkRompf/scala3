package typestate

import language.experimental.captureChecking
import caps.*
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation
object FUN

class myCap:
  type pathCap

// we must put each subtyping error in separate file, since if subtyping error compilation stops.
object KillSubNeg1:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def covar(): ((x: myCap^) => Unit @kill(x)) => (myCap^ => Unit) =
    0.asInstanceOf[((lll: myCap^) => Unit @kill(lll) => ((jjj: myCap^) => Unit @kill(jjj)))] // error

