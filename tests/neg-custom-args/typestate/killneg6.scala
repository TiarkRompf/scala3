import typestate.*

class myCap:
  type pathCap

object KillNeg4:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def negHO1() =
    val f: myCap^ = new myCap
    val f1 = f
    val g = () => kmyCap(f); ()
    g()
    val j = f1 // error

  def upperbounded(): Unit =
    val z: myCap^ = new myCap
    def inner(): Unit @kill(z) = () // error

  def paramDep(f: myCap^, y: () => Unit @kill(f)) =
    y()
    val a = 0239

  def wow() =
    val k: myCap^ = new myCap
    paramDep(k, () => ())
    val g = k // error

