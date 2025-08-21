import typestate.*

class myCap:
  type pathCap

object KillNeg2:
  def kmyCap(aba: myCap^): Unit @kill(aba) = ()

  def accountsFor(x: myCap^, y: myCap^, z: myCap^): Unit = // error
   kmyCap(x)

  def accountsFor2(x: myCap^, y: myCap^, z: myCap^): Unit @kill(y) = // error
   kmyCap(x)
   kmyCap(y)

  def testIf(f: myCap^, b: Boolean) =
    val a: myCap^ = new myCap
    val g: myCap^ = new myCap
    if (b && kmyCap(a).isInstanceOf[Unit]) then
      kmyCap(f)
      val j = a // error
    else
      val j = f
      kmyCap(g)
    val k = f // error
    val l = g // error

  def testIf2(f: myCap^, b: Boolean, c: Boolean, d: Boolean) =
    val a: myCap^ = new myCap
    val g: myCap^ = new myCap
    if (b) {
      kmyCap(f)
      val j = a
    }
    else if (c) {
      kmyCap(a)
      val k = g
    }
    else if (d) {
      kmyCap(g)
      val o = f
    }
    else {
      val b1 = g
      val b2 = a
      val b3 = f
    }
    val hm = a // error
    val hm2 = g // error
    val hm3 = f // error

  def testApp(): Unit =
    val j: myCap^ = new myCap
    ((x: myCap^) => kmyCap(x))(j)
    val l = j // error

  def retKill1(): (asdf: myCap^) => Unit @kill(asdf) =
    val g = (f: myCap^) =>
      kmyCap(f)
      val dummy = 29
    g

  def useRet1() =
    val l1: myCap^ = new myCap
    retKill1()(l1)
    val bad = l1 // error

  def retKill2() =
    def foo(f: myCap^) =
      kmyCap(f)
      val dummy = 29
    foo

  def useRet2() =
    val l2: myCap^ = new myCap
    val k = retKill1()
    val j = k
    k(l2)
    val bad = l2 // error

  def defConforms(): (Int) => (myCap^ => Unit) =
    val g = (f: myCap^) => kmyCap(f) // error
    (x: Int) => g

  def inferTestPoly[A, B, C](x: A, Y: B | C, z: myCap^) =
    kmyCap(z)
    x

  def callInferTestPoly() =
    val j: myCap^ = new myCap
    inferTestPoly(1, 2, j)
    val k = j // error






