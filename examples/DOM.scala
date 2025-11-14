import scala.concurrent.{ Future, ExecutionContext }
import ExecutionContext.Implicits.global
import language.experimental.captureChecking
import typestate.*

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
  class ::[E <: Elem, L <: EList] extends EList

  class DOM private[ManualDOM]():
    type Elems[T <: EList]

  extension (tree: DOM)
    def open[E <: Elem, L <: EList](elem: E): tree.Elems[L] ?=!>? tree.Elems[E :: L] =
      Sigma((), ().asInstanceOf[tree.Elems[E :: L]])

    def close[E <: Elem, L <: EList](elem: E): tree.Elems[E :: L] ?=!>? tree.Elems[L] =
      Sigma((), ().asInstanceOf[tree.Elems[L]])

    def text[E <: Elem, L <: EList](elem: E, s: String): tree.Elems[E :: L]^ ?=> Unit =
      ()

  def makeDOM(body: (tree: DOM) => (tree.Elems[ENil]^) =!> (tree.Elems[ENil]^) ?<= Unit): Unit =
    val dom = new DOM:
      type Elems[ENil] = Unit
    body(dom)(())

  object DOM:
    def apply(): Sigma { type A = DOM; type B = a.Elems[ENil]^ } =
      val dom = new DOM:
        type Elems[ENil] = Unit
      new Sigma {
        type A = DOM
        type B = a.Elems[ENil]^
        val a: dom.type = dom
        val b: dom.Elems[ENil]^ = ()
      }
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

def move[T]: T ?=!>? T =
  Sigma((), summon[T].asInstanceOf[T])

def loop[T](using c: T^)(cond: => Boolean)(body: T ?=!>? T): ((T^) ?<= Unit) @kill(c) =
  if cond then
    body(using c)
    loop[T](cond)(body)
  else move[T]

object Main:
  import ManualDOM.*

  def nextTR[L <: EList](tree: DOM): tree.Elems[TR :: L] ?=!>? tree.Elems[TR :: L] =
    tree.close(TR())
    tree.open(TR())

  def twoCells[L <: EList](tree: DOM, fst: String, scd: String): tree.Elems[TR :: L] ?=!>? tree.Elems[TR :: L] =
    tree.open(TD())
    tree.text(TD(), fst)
    tree.close(TD())
    tree.open(TD())
    tree.text(TD(), scd)
    tree.close(TD())

  def main1() =
    makeDOM { dom => ts =>
      dom.open(TABLE())(using ts)
      dom.open(TBODY())
      dom.open(TR())

      val response = await(fetch("/api/logs").toFuture)
      val reader = response.body.getReader()

      type TStart = TR :: TBODY :: TABLE :: ENil

      def readAll(dom: DOM): dom.Elems[TStart] ?=!>? dom.Elems[ENil] =
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

  def main2() =
    makeDOM { dom => ts =>
      dom.open(TABLE())(using ts)
      dom.open(TBODY())
      dom.open(TR())

      val response = await(fetch("/api/logs").toFuture)
      val reader = response.body.getReader()

      type TStart = TR :: TBODY :: TABLE :: ENil
      var chunk = await(reader.read().toFuture)

      loop[dom.Elems[TStart]](!chunk.isDone) {
        val line = chunk.value
        twoCells(dom, line.timeStamp, line.message)
        chunk = await(reader.read().toFuture)
      }

      dom.close(TR())
      dom.close(TBODY())
      dom.close(TABLE())
    }

  def test1() =
    makeDOM { tree => ts =>
      tree.open(HTML())(using ts)
      tree.open(HEAD())
      tree.text(HEAD(), "a")
      tree.open(P())
      tree.open(P())
      tree.close(P())
      tree.close(P())
      // tree.close(P()) // fail
      tree.close(HEAD())
      tree.close(HTML())
    }

  // def test2() =
  //   makeDOM { tree => ts =>
  //     implicit val _ts = ts
  //     tree.open(HTML())
  //     tree.open(HEAD())
  //     tree.close(HEAD())
  //     val j = 20
  //   }
