import typestate.*

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

  def subFree() =
    val x: myCap^ = new myCap
    val y = x
    def g(func: () => Unit @kill(x)) = ???
    val func2: () => Unit @kill(y) = ???
    g(func2)

  def etaExpand() =
    val l: myCap^ = new myCap

    def f1(func: () => Unit @kill(FUN)): Unit @kill(func) =
      ()

    def f2() = kmyCap(l)
    f1(f2)

  def notCap() =
    val l: myCap = ???
    kmyCap(l)

  def upperBounded(): Unit =
    val j: myCap^ = new myCap
    val k = j
    val p = k

    def one(): Unit @kill(j) =
      val aa = p

    def two(): Unit @kill(p) =
      val bb = j
