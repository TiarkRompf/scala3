import typestate.*

class myCap:
  type pathCap

object KillNeg4:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def subFree() =
    val l: myCap^ = new myCap
    val y: myCap^ = new myCap
    def func(f: () => Unit @kill(l)) = ???
    val g = () => kmyCap(y)
    func(g) // error
    ()

  def subFree2() =
    val x: myCap^ = new myCap
    val y = x
    def g(func: () => Unit @kill(y)) = ???
    val func2: () => Unit @kill(x) = ???
    g(func2) // error

  def etaExpand() =
      val l: myCap^ = new myCap

      def f1(func: () => Unit @kill(FUN)): Unit @kill(func) =
        ()

      def f2(): Unit @kill(FUN) = kmyCap(l)

      f1(f2)
      f2() // error

