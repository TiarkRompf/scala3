import language.experimental.captureChecking
import caps.*
import typestate.*

class File(val name: String):
  type IsClosed
  type IsOpen

object File:
  def apply(name: String): Sigma { type A = File; type B = a.IsClosed^ } =
    val f = new File(name):
      type IsClosed = Unit
      type IsOpen = Unit
    new Sigma:
      type A = File
      type B = a.IsClosed^
      val a: f.type = f
      val b: f.IsClosed^ = ()

  extension (f: File)
    def open()(using c: f.IsClosed^): (Unit `Pair` (f.IsOpen^)) @kill(c) =
      new Sigma:
        type A = Unit
        type B = f.IsOpen^
        val a = ()
        val b: f.IsOpen^ = ().asInstanceOf[f.IsOpen]

    def close()(using c: f.IsOpen^): (Unit `Pair` (f.IsClosed^)) @kill(c) =
      new Sigma:
        type A = Unit
        type B = f.IsClosed^
        val a = ()
        val b: f.IsClosed^ = ().asInstanceOf[f.IsClosed]

    def read()(using f.IsOpen^): String = ""

    def write(s: String)(using f.IsOpen^): Unit = ()

  def withFile[T](name: String)(body: (f: File) => (c: f.IsClosed^) => (`Pair`[T, f.IsClosed^]^) @kill(c)): T =
    val f = new File(name):
      type IsClosed = Unit
      type IsOpen = Unit
    ((body(f)(())) : `Pair`[T, f.IsClosed^]^).a

  def withFileM(name: String)(body: (f: File) => (c: f.IsClosed^) => ((`Pair`[Unit, f.IsClosed^])^) @kill(c)): Unit =
    val f = new File(name):
      type IsClosed = Unit
      type IsOpen = Unit
    body(f)(())

object Main:
  import File.*

  // def test1() =
  //   val f = File("a.txt")
  //   f.open()
  //   val msg = f.read()
  //   f.write("Hello")
  //   f.close()
  //   // f.write("BAD")

  def test2(messages: List[String]) =
    val f = File("a.txt")
    f.open()
    for msg <- messages do
      f.write(msg)
    f.close()

  def test4() =
    val text = withFile("a.txt") { (f) => (c) =>
      f.open()(using c)
      val msg = f.read()
      f.write("asdf")
      f.close()
      msg
    }

  // def test4b() =
  //   withFile("a.txt") { (f) => (c) =>
  //     f.open()(using c)
  //     val msg = f.read()
  //     f.write("Hello")
  //     f.close()
  //     new Sigma {
  //       type A = String
  //       type B = f.IsClosed^
  //       val a = msg
  //       val b: f.IsClosed^ = summon[f.IsClosed^]
  //     }
  //   }


