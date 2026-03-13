import language.experimental.captureChecking
import caps.*
import scala.io.Source
import java.io.{FileWriter => JWriter}
import typestate.*

class File private(val path: String):
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

object Main:
  import File.*

  def test1(): Unit =
    val f = File("a.txt")
    f.open()
    val msg = f.read()
    f.write("Hello")
    f.close()
    // f.write("BAD")

  def test2(messages: List[String]): Unit =
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

  def main(args: Array[String]): Unit =
    val f = File("examples/sample.txt")
    val messages = Array("Hello", "World", "This", "Is", "An", "Array")
    f.open()
    for msg <- messages do
      f.write(msg)
    f.close()