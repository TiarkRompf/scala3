object Main:

  class Cap

  def open(): Cap = new Cap

  def write(s: String)(using Cap): Unit = println(s)

  def close()(using Cap): Unit = ()

  def main(args: Array[String]): Unit =

      open()  // error if commented out

      // open()  // error if commented in together with first

      write("Hello Scalac!")

      close()

      // write("Hello Scalac!")  // error if commented in

      () // need explicit return val right now