import scala.concurrent.{ Future, ExecutionContext }
import ExecutionContext.Implicits.global

package ManualDOM:
  trait Elem(val name: String)
  class HTML() extends Elem("<html>")
  class HEAD() extends Elem("<head>")
  class TITLE() extends Elem("<title>")
  class BODY() extends Elem("<body>")
  class P() extends Elem("<p>")
  class TR() extends Elem("<tr>")
  class TD() extends Elem("<td>")
  class TABLE() extends Elem("<table>")
  class TBODY() extends Elem("<tbody>")
  class UL() extends Elem("<ul>")
  class LI() extends Elem("<li>")

  trait EList
  class ENil extends EList
  class ::[E <: Elem, L <: EList]

  class DOM private[ManualDOM]()

  extension (tree: DOM)
    def open[E <: Elem](elem: E): Unit =
      ()

    def close[E <: Elem](elem: E): Unit =
      ()

    def text[E <: Elem](elem: E, s: String): Unit =
      ()

  def makeDOM(body: (tree: DOM) => Unit): Unit =
    val dom = new DOM()
    body(dom)

  object DOM:
    def apply(): DOM =
      new DOM()
end ManualDOM

// Dummy classes simulating fetch API
class Line:
  def timeStamp: String = ???
  def message: String = ???

class JSPromise[T]:
  def toFuture: Future[T] = ???

class Chunk:
  def isDone: Boolean = ???
  def value: Line = ???

class ReadableStreamReader:
  def read(): JSPromise[Chunk] = ???

class ReadableStream:
  def getReader(): ReadableStreamReader = ???

class Response:
  val body: ReadableStream = ???

def fetch(s: String): JSPromise[Response] = ???
def await[T](f: Future[T]): T = ???

object Main:
  import ManualDOM.*

  def nextTR(tree: DOM): Unit =
    tree.close(TR())
    tree.open(TR())

  def twoCells(tree: DOM, fst: String, scd: String): Unit =
    tree.open(TD())
    tree.text(TD(), fst)
    tree.close(TD())
    tree.open(TD())
    tree.text(TD(), scd)
    tree.close(TD())

  def main1() =
    makeDOM { dom =>
      dom.open(TABLE())
      dom.open(TBODY())
      dom.open(TR())

      val response = await(fetch("/api/logs").toFuture)
      val reader = response.body.getReader()

      def readAll(dom: DOM): Unit =
        val chunk = await(reader.read().toFuture)
        if !chunk.isDone then
          val line = chunk.value
          twoCells(dom, line.timeStamp, line.message)
          nextTR(dom)
          readAll(dom)
        else
          dom.close(TR())
          dom.close(TBODY())
          dom.close(TABLE())
      readAll(dom)
    }

  def test1() =
    makeDOM { tree =>
      tree.open(HTML())
      tree.open(HEAD())
      tree.text(HEAD(), "a")
      tree.open(P())
      tree.open(P())
      tree.close(P())
      tree.close(P())
      tree.close(HEAD())
      tree.close(HTML())
    }

  def test2() =
    makeDOM { tree =>
      tree.open(HTML())
      tree.open(HEAD())
      tree.open(TITLE())
      tree.open(BODY())
      tree.open(P())
      tree.open(HEAD())
      tree.open(TITLE())
      tree.open(BODY())
      tree.open(P())
      tree.open(HEAD())
      tree.open(TITLE())
      tree.open(BODY())
      tree.open(P())
      tree.open(HEAD())
      tree.open(TITLE())
      tree.open(BODY())
      tree.open(P())

      tree.close(P())
      tree.close(BODY())
      tree.close(TITLE())
      tree.close(HEAD())
      tree.close(P())
      tree.close(BODY())
      tree.close(TITLE())
      tree.close(HEAD())
      tree.close(P())
      tree.close(BODY())
      tree.close(TITLE())
      tree.close(HEAD())
      tree.close(P())
      tree.close(BODY())
      tree.close(TITLE())
      tree.close(HEAD())

      tree.close(HTML())
    }

  def test3() =
    makeDOM { tree =>
      tree.open(HTML())
      tree.open(HEAD())
      tree.open(TITLE())
      tree.open(BODY())
      tree.open(P())
      tree.open(HEAD())
      tree.open(TITLE())
      tree.open(BODY())
      tree.open(P())
      tree.open(HEAD())
      tree.open(TITLE())
      tree.open(BODY())
      tree.open(P())
      tree.open(HEAD())
      tree.open(TITLE())
      tree.open(BODY())
      tree.open(P())

      tree.close(P())
      tree.close(BODY())
      tree.close(TITLE())
      tree.close(HEAD())
      tree.close(P())
      tree.close(BODY())
      tree.close(TITLE())
      tree.close(HEAD())
      tree.close(P())
      tree.close(BODY())
      tree.close(TITLE())
      tree.close(HEAD())
      tree.close(P())
      tree.close(BODY())
      tree.close(TITLE())
      tree.close(HEAD())

      tree.close(HTML())
    }