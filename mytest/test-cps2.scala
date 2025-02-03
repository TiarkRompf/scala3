object Main:

  class uncps extends scala.annotation.StaticAnnotation

  def reify[A](x: CPS[A] @uncps): CPS[A] @uncps = x

  class CPS[A](val fun: [B] => (A=>B) => B) {
    def flatMap[B](f: A => CPS[B]): CPS[B] = 
      CPS([C] => k => fun(x => reify(f(x)).fun(k)))
  }

  implicit def shiftUnit[T](x: T): CPS[T] = new CPS([B] => k => k(x))

  def log[A](x: A): A = { println("log "+x); x}


  def bing(x: Int): CPS[Int] = shiftUnit(log(x))


  
  def main(args: Array[String]): Unit =

    val res = reify(test2())
    println("result: " + res.fun(x => x))


  def test2() = //: CPS[Unit] =

    println("A")

    1 + bing(10) + bing(20)

    val z = bing(30)

    bing(40) // warning about pure expression: mark sym as InlineParam?

    println("B")


  // TODO: this isn't correct yet
  // def test3() = 1 + bing(50)
