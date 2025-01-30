object Main:

  // provided by library (scala.typestate.eff)
  class eff

  def open(): Unit @eff

  def write(s: String): Unit @eff

  def close()(: Unit @eff


  // Level 0.5

  def func(x: Int) = { // infer result type:  Unit @eff

    open()
    write()
    close()

  }


  def g() = { // infer result: Unit @eff

    f()

  }


  def h(): Unit = { 

    g()  // error

  }



  // Level 0.7

  def higherorder(f: Int => Int @eff) = { // infer result: Int @eff

    f(7)

  }


  def higherorder_pure(f: Int => Int) = {

    f(7)

  }


  def useHO() = {

    higherorder(func) // ok

    higherorder_pure(func) // error

  }




  def main(args: Array[String]): Unit =

