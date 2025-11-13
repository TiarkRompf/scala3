import language.experimental.captureChecking
import caps.*
import scala.io.Source
import java.io.{FileWriter => JWriter}
import typestate.*

class File(val path: String):
  type IsClosed
  type IsOpen

  private var reader: Option[Source] = None
  private var writer: Option[JWriter] = None
  private var lineIter: Option[Iterator[String]] = None

object File:
  def apply(path: String): Sigma { type A = File; type B = a.IsClosed^ } =
    val f = new File(path):
      type IsClosed = Unit
      type IsOpen = Unit
    new Sigma:
      type A = File
      type B = a.IsClosed^
      val a: f.type = f
      val b: f.IsClosed^ = ()

  extension (f: File)
    def open(): f.IsClosed ?=!>? f.IsOpen =
      f.reader = Some(Source.fromFile(f.path))
      f.lineIter = f.reader.map(_.getLines())
      f.writer = Some(new JWriter(f.path, true))
      Sigma((), ().asInstanceOf[f.IsOpen])

    def close(): f.IsOpen ?=!>? f.IsClosed =
      f.reader.foreach(_.close())
      f.writer.foreach(_.close())
      f.reader = None
      f.writer = None
      f.lineIter = None
      Sigma((), ().asInstanceOf[f.IsClosed])

    def read(): f.IsOpen^ ?=> Option[String] =
      f.lineIter.flatMap(it => if (it.hasNext) then Some(it.next()) else None)

    def write(line: String): f.IsOpen^ ?=> Unit =
      f.writer.foreach { w =>
        w.write(line)
        w.write("\n")
        w.flush()
      }

  def withFile[T](path: String)(body: (f: File) => (f.IsClosed^) =!> (f.IsClosed^) ?<= T): T =
    val f = new File(path):
      type IsClosed = Unit
      type IsOpen = Unit
    ((body(f)(())) : ((f.IsClosed^) ?<= T)).a

def ifC[T, B1, B2](using c: T^)(cond: => Boolean)[A1](tbranch: (T^) ?=!> ((B1^) ?<= A1))(ebranch: (T^) ?=!> ((B2^) ?<= A1)): ((Either[B1, B2]^) ?<= A1) @kill(c) =
  if cond then
    val sigma = tbranch : ((B1^) ?<= A1)
    val a1: sigma.a.type = sigma.a
    val b1 = sigma.b
    Sigma(a1, Left(b1.asInstanceOf[B1]))
  else
    val sigma = ebranch : ((B2^) ?<= A1)
    val a1: sigma.a.type = sigma.a
    val b2 = sigma.b
    Sigma(a1, Right(b2.asInstanceOf[B2]))

def leftC[T](using c: T^): ((Either[T^, Nothing]^) ?<= Unit) @kill(c) = ???

def rightC[U](using c: U^): ((Either[Nothing, U^]^) ?<= Unit) @kill(c) = ???

def matchC[A, B, T](using c: Either[A^, B^]^)[U](left: (A^) ?=!> ((T^) ?<= U))(right: (B^) ?=!> ((T^) ?<= U)): ((T^) ?<= U) @kill(c) =
  ???
  // c match
  //   case Left(b1) =>
  //     left[A](using b1)
  //   case Right(b2) =>
  //     right[B](using b2)

  /**
   * Limitations:
   * 1. Can only kill one capability throughout
   * 2. Cannot express "or", must return the same capability at the end -> see session types
   */
  // def loop[T, U](using c: T^)(body: T ?=!>? Either[T, U]): (U^) ?<= Unit @kill(c) =
  //     val b = (body : Sigma).b
  //     b match
  //       case Some(c) =>
  //         loop[T](using c)(body)
  //       case None =>

      // implicit f.IsOpen
      // summon[Either]

object Main:
  import File.*

  // def testLoop() =
  //   val f = File("b.txt")
  //   loop[f.IsClosed](true) {
  //     f.open()
  //     f.close()
  //     val j = 23847
  //   }

  def testIf() =
   withFile("a.txt") { f => c =>
    f.open()(using c)
    f.close()
    ifC[f.IsClosed, f.IsClosed, f.IsOpen] (true) {
      f.open()
      f.close() // = 212
    } {
      f.open()
    }
    val k = 201
    matchC[f.IsClosed, f.IsOpen, f.IsOpen] {
      val k = 2397
      f.open()
    } {
      f.close()
      f.open()
    }
    f.close()
   }


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

  // def test3(messages: Array[String]) =
  //   val f = File("b.txt")
  //   f.open()
  //   var i = 0
  //   while (i < messages.length) do
  //     f.write(messages(i))
  //     f.close()
  //   f.close()

  // def test4() =
  //   val text = withFile("a.txt") { (f) => (c) =>
  //     f.open()(using c)
  //     val msg = f.read()
  //     f.write("asdf")
  //     f.close()
  //     msg
  //   }

  // def test5() =
  //   withFile("a.txt") { (f) => (c) =>
  //     {
  //       f.open()(using c)
  //     }
  //     // if ??? then
  //     //   val j = 2938
  //     //   f.open()(using c)
  //     // else
  //     //   val f = 123
  //     //   f.open()(using c)

  //     f.write("asdf")
  //     f.close()
  //   }

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

  def main(args: Array[String]): Unit =
    val f = File("ts-test/abc.txt")
    val messages = Array("Hello", "World", "This", "Is", "A", "Test")
    f.open()
    for msg <- messages do
      f.write(msg)
    f.close()