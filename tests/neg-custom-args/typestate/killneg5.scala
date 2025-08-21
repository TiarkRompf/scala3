import typestate.*

class myCap:
  type pathCap

object KillNeg4:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def outer() =
    def avoid() =
      val f: myCap^ = new myCap
      () =>
        kmyCap(f)
        ()

    def asdf() =
      val g = avoid()
      g()
      g() // error

  def outer2() =
    def hoo(f: myCap^) =
      (g: myCap^) =>
        kmyCap(f)

    def useHoo() =
      val f: myCap^ = new myCap
      hoo(f)(new myCap)
      val k = f // error

    def useHoo2() =
      val f: myCap^ = new myCap
      val g = hoo(f)
      g(new myCap)
      g(new myCap) // error