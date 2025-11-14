import typestate.*

class File(val name: String):
  type isClosed
  type isOpen

object File:
  def open(f: File, c: f.isClosed^): (f.isOpen^) @kill(c) =
    1.asInstanceOf[f.isOpen]

  def close(f: File, c: f.isOpen^): (f.isClosed^) @kill(c) =
    0.asInstanceOf[f.isClosed]

  def read(f: File, c: f.isOpen^): String = "TODO"

  def write(f: File, s: String, c: f.isOpen^): Unit = ()

  def withFile[T](name: String)(body: (f: File) => (c: f.isClosed^) => ((T, f.isClosed^) @kill(c))): T =
    val file = new File(name):
      type isClosed = Int
      type isOpen = Int
    body(file)(0)._1

object FileMainPos:
  import File.*
  def test1() =
    withFile("a.txt") { (f) => (c) =>
      val o = open(f, c)
      val o2 = o
      val o3 = o2

      val msg = read(f, o3)
      write(f, "Hello World", o3)
      val c2 = close(f, o)

      // val bad = read(f, o3)
      (msg, c2)
    }

  def test2(messages: List[String]) =
    withFile("a.txt") { (f) => (c) =>
      val o = open(f, c)

      for msg <- messages do
        write(f, msg, o)

      messages.foreach(msg => write(f, msg, o))

      val c2 = close(f, o)
      ((), c2)
    }


