object Main:

  class CPS[A](val fun: [B] => (A=>B) => B) {
    def flatMap[B](f: A => CPS[B]): CPS[B] = 
      CPS([B] => k => fun(x => f(x).fun(k)))
  }


  implicit def shiftUnit[T](x: T): CPS[T] = new CPS([B] => k => k(x))

  def bing(x: Int): CPS[Int] = shiftUnit(x)


  
  def main(args: Array[String]): Unit =

    val res = test2()
    println("result: " + res.fun(x => x))


  // def test1() = 
  //   bing(10)

  //   bing(20)

  //   bing(30)

  //   ()


  def test2(): CPS[Unit] =

    println("A")

    1 + bing(10) + bing(20)

    val z = bing(30)

    bing(40)

    // print(cps0)

    println("B")
