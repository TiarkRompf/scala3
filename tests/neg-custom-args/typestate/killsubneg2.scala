import typestate.*

class myCap:
  type pathCap

object KillSubNeg2:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def contra(): ((x: myCap^) => Unit @kill(x)) => Unit =
    0.asInstanceOf[(myCap^ => Unit) => Unit] // error