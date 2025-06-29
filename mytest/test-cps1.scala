object Main:

  class uncps extends scala.annotation.StaticAnnotation

  def reify[A](x: CPS[A] @uncps): CPS[A] @uncps = x

  class CPS[A](val fun: [B] => (A=>B) => B) {
    def flatMap[B](f: A => CPS[B]): CPS[B] = 
      CPS([B] => k => fun(x => reify(f(x)).fun(k)))
  }


  implicit def shiftUnit[T](x: T): CPS[T] = new CPS([B] => k => k(x))

  def log[A](x: A): A = { println("log "+x); x}


  def bing(x: Int): CPS[Int] = shiftUnit(log(x))


  
  def main(args: Array[String]): Unit =

    val res = reify(test1())
    println("result: " + res.fun(x => x))


  def test1() = //: CPS[Int] =
    bing(10)

    bing(20)

    bing(30)