import typestate.*

class myCap:
  type pathCap

object KillNeg7:
  def fudge(f: () => Unit @kill(FUN))(using v: Int): Unit @kill(f) = ???

  def testFudge() =
    val j: myCap^ = ???
    implicit val o: Int = 23
    fudge { () =>
      val dummy = j
    }
    val k = j // error

  def killPoly1[T <: myCap](x: T): Unit @kill(x) = ??? // error
  def killPoly2[T](x: T): Unit @kill(x) = ??? // error
  def killPoly3[T](x: T, y: x.type): Unit @kill(y) = ??? // error

  def killPolyFree[T](x: T) =
    val hmm: () => Unit @kill(x) = () => // error
      val k = x
      val j = ()

  def killPolyGood[T](x: T^): Unit @kill(x) = ???

  def killPolyArg[T](x: T) =
    killPolyGood[T](x) // error



