import typestate.*

class myCap:
  type pathCap

object KillSubNeg3:
  def loopCPS(f: myCap^)
  (body: ((fK: myCap^) => Unit @kill(fK)) -> (f2: myCap^) => Unit @kill(f2)): Unit @kill(f) =
    val K: (f3: myCap^) -> Unit @kill(f3) =
      f => loopCPS(f)(body)
    body(K)(f)

  def testLoopCPS(b: String): Unit =
    val g: myCap^ = new myCap
    loopCPS(g) { (K: myCap^ => Unit) => (f) => // error
      ()
    }