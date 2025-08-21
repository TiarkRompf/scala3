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