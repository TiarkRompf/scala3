import language.experimental.captureChecking
import caps.*
import typestate.*

// class kill(xs: Any*) extends annotation.StaticAnnotation
// object FUN

// trait Sigma {
//   type A
//   type B
//   val a: A
//   val b: B
// }

// type `Pair`[A1, B1] = Sigma { type A = A1; type B = B1 }

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

  // instantiating type vars is done in Inferencing.scala instantiateTypeArgs
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

  // def test2(messages: List[String]) =
  //   val f = File("a.txt")
  //   f.open()
  //   for msg <- messages do
  //     f.write(msg)
  //   f.close()

  // Problem 1 - the new Sigma captures the sigma1_CAP capability.
  // Problem 2 - separation failure since f.IsClosed^ hides non-local sigma1_CAP (in enclosing cap).
  // def test3() =
  //   withFileM("a.txt") { (f) => (c) =>
  //     f.open()(using c)
  //     val msg = f.read()
  //     f.write("Hello")
  //     f.close()
  //     val fo = 2383
  //     // new Sigma:
  //     //   type A = Unit
  //     //   type B = f.IsClosed^
  //     //   val a = ()
  //     //   val b = summon[f.IsClosed^]
  //   }

  def test4() =
    val text = withFile("a.txt") { (f) => (c) =>
      f.open()(using c)
      val msg = f.read()
      f.write("asdf")
      f.close()
      msg
    }

  // def test4() =
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

  // def test1() =
  //   withFile("a.txt") { (f) => (c) =>
  //     val o = open(f, c)
  //     val o2 = o
  //     val o3 = o2

  //     val msg = read(f, o3)
  //     write(f, "Hello World", o3)
  //     val c2 = close(f, o3)

  //     write(f, "Hello World", o)
  //     (msg, c2)
  //   }

  // def test2(messages: List[String]) =
  //   withFile("a.txt") { (f) => (c) =>
  //     val o = open(f, c)

  //     for msg <- messages do
  //       write(f, msg, o)

  //     messages.foreach(msg => write(f, msg, o))

  //     val c2 = close(f, o)
  //     ((), c2)
  //   }


