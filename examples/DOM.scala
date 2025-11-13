import scala.concurrent.{ Future, ExecutionContext }
import ExecutionContext.Implicits.global
import language.experimental.captureChecking
import caps.cap
import typestate.*

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

class DOM:
  type Elems[T <: EList]

object DOM:
  extension (tree: DOM)
    // find way to avoid elem term parameter?
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

  def apply(): Sigma { type A = DOM; type B = a.Elems[ENil]^ } = ???

  // def makeDOM(body: (tree: DOM) => (ts: tree.Elems[ENil]^) =>
  //     (Sigma { type A = Unit; type B = tree.Elems[ENil]^{ts, cap} }^{ts, cap}) @kill(ts)): Unit =
  //   val dom = new DOM:
  //     type Elems[ENil] = Unit
  //   body(dom)(())

class Line:
  def timeStamp: String = ???
  def message: String = ???

class Promize:
  def done: Boolean = ???
  def value: Line = ???

class ReadableStreamReader:
  def read(): Promize = ???

class ReadableStream:
  def getReader(): ReadableStreamReader = ???

class Response:
  val body: ReadableStream = ???

object Fetch:
  def fetch(url: String): Future[Response] = ???

extension [T](future: Future[T])
  def foreachK[U](f: T => U @kill(FUN))(using executor: ExecutionContext): Unit @kill(f) = ???

def await[T](f: Future[T]): T = ???

def loop[T](using c: T^)(cond: => Boolean)(body: (p: T^) ?=> (Sigma { type A = Unit; type B = T^{cap, p}}^{cap, p}) @kill(p)): Unit @kill(c) =
  ???
  // if cond then
  //   body : (Sigma { type A = Unit; type B = T^{cap, c}})^{cap, c}
  //   loop[T](cond)(body)

// def fixState[T](using c: T^)(body: (p: T^) ?=> (Sigma { type A = Unit; type B = T^{cap, p}}^{cap, p}) @kill(c, FUN)): Unit @kill(c, body) = ???

class Hack

object Main:
  import DOM.*

  def newTableRow[L <: EList](tree: DOM): tree.Elems[TR :: L] ?=!>? tree.Elems[TR :: L] =
    tree.close(TR())
    tree.open(TR())

  def addTwoTC[L <: EList](tree: DOM, texts: (String, String)): tree.Elems[TR :: L] ?=!>? tree.Elems[TR :: L] =
    tree.open(TD())
    tree.text(TD(), texts._1)
    tree.close(TD())
    tree.open(TD())
    tree.text(TD(), texts._2)
    tree.close(TD())

  // def test2() =
  //   makeDOM { tree => ts => // ts: tree.Elems[ENil]
  //     tree.open(TABLE())(using ts) // tree.Elems[TABLE :: ENil]
  //     tree.open(TR()) // tree.Elems[TABLE :: ENil] >> tree.Elems[TR :: TABLE :: ENil]
  //     addTwoTC(tree, "cell1", "cell2") // stays tree.Elems[TR :: TABLE :: ENil]
  //     newTableRow(tree) // stays tree.Elems[TR :: TABLE :: ENil]
  //     addTwoTC(tree, "cell3", "cell4")
  //     tree.close(TR()) // tree.Elems[TR :: TABLE :: ENil] >> tree.Elems[TABLE :: ENil]
  //     tree.close(TABLE()) // tree.Elems[TABLE :: ENil] >> tree.Elems[ENil]
  //   }

  // def test1() =
  //   makeDOM { tree => ts =>
  //     tree.open(HTML())(using ts)
  //     tree.open(HEAD())
  //     tree.text(HEAD(), "a")
  //     tree.open(P()) // tree.Elems[P :: ]
  //     tree.open(P()) // tree.Elems[P :: ]
  //     tree.close(P())
  //     tree.close(P())
  //     // tree.close(P()) // fail
  //     tree.close(HEAD())
  //     tree.close(HTML())
  //   }

  // def streaming() =
  //   makeDOM { (dom: DOM) => ts =>
  //     dom.open(TABLE())(using ts)
  //     dom.open(TBODY())
  //     dom.open(TR())

  //     Fetch.fetch("/api/logs/stream").foreachK { response =>
  //       val reader = response.body.getReader()
  //       var done = false

  //       loop[dom.Elems[TR :: TBODY :: TABLE :: ENil]](done) {
  //         val chunk = reader.read() // this should be asynchronous
  //         if (!chunk.done) then
  //           val line = chunk.value
  //           addTwoTC(dom, (line.timeStamp, line.message))
  //           newTableRow(dom) // dom.close(TR), dom.open(TR)
  //         else
  //           done = true
  //       }
  //     }
  //     dom.close(TR())
  //     dom.close(TBODY())
  //     dom.close(TABLE())
  //   }

  // def streaming2() =
  //   makeDOM { (dom: DOM) => ts =>
  //     implicit val _ts = ts
  //     dom.open(TABLE())
  //     dom.open(TBODY())
  //     dom.open(TR())

  //     Fetch.fetch("/api/logs/stream").foreachK { response => // [T => U] Sigma { type A = }
  //       val reader = response.body.getReader()
  //       var done = false

  //       def readNext(): (dom.Elems[TR :: TBODY :: TABLE :: ENil]^) ?=!>? (dom.Elems[ENil]^) =
  //         val chunk = reader.read() // should also be a Future
  //         if (!chunk.done) then
  //           val line = chunk.value
  //           addTwoTC(dom, (line.timeStamp, line.message))
  //           newTableRow(dom)
  //           readNext()
  //         else
  //           dom.close(TR())
  //           dom.close(TBODY())
  //           dom.close(TABLE())

  //       readNext() : Sigma { type A = Unit; type B = dom.Elems[ENil]^ }
  //     }

  //     // dom.close(TR())
  //     // dom.close(TBODY())
  //     // dom.close(TABLE())
  //   }


  // def streaming3() =
  //   val dom = DOM()
  //   dom.open(TABLE())
  //   dom.open(TBODY())
  //   dom.open(TR())

  //   Fetch.fetch("/api/logs/stream").foreachK { response =>
  //     val reader = response.body.getReader()
  //     var done = false

  //     def readNext(): (dom.Elems[TR :: TBODY :: TABLE :: ENil]^) ?=!> Unit =
  //       val chunk = reader.read()
  //       if (!chunk.done) then
  //         val line = chunk.value
  //         addTwoTC(dom, (line.timeStamp, line.message))
  //         newTableRow(dom)
  //         readNext()
  //       else
  //         dom.close(TR())
  //         dom.close(TBODY())
  //         dom.close(TABLE())

  //     readNext()
  //   }

  def loop[T, U](using c: T^)(body: T ?=!>? Either[T, U]): ((U^) ?<= Unit) @kill(c) = ???

  def ifC[T, B1, B2](using c: T^)(cond: => Boolean)[A1](tbranch: (T^) ?=!> ((B1^) ?<= A1))(ebranch: (T^) ?=!> ((B2^) ?<= (A1))): ((Either[B1, B2]^) ?<= A1) @kill(c) = ???

  def matchC[A, B, T](using c: Either[A^, B^]^)[U](left: (A^) ?=!> ((T^) ?<= U))(right: (B^) ?=!> ((T^) ?<= U)): ((T^) ?<= U) @kill(c) = ???

  def merge[T]: Either[T, T] ?=!>? T = ???

  def noChange[T]: T ?=!>? T =
    val t = summon[T]
    Sigma((), t.asInstanceOf[T])

  def whileI[T](using c: T^)(cond: => Boolean)(body: T ?=!>? T): ((T^) ?<= Unit) @kill(c) =
    if cond then
      body
      whileI[T](cond)(body)


  // type TStart = TR :: TBODY :: TABLE :: ENil

  // def streaming4() =
  //   makeDOM { dom => ts =>
  //     dom.open(TABLE())(using ts)
  //     dom.open(TBODY())
  //     dom.open(TR())

  //     def readAll(): dom.Elems[TStart] ?=!>? dom.Elems[ENil] =
  //       ifC[dom.Elems[TStart], dom.Elems[ENil], dom.Elems[ENil]](true) {
  //         addTwoTC(dom, ???)
  //         newTableRow(dom)
  //         readAll()
  //       } {
  //         dom.close(TR())
  //         dom.close(TBODY())
  //         dom.close(TABLE())
  //       }
  //       merge[dom.Elems[ENil]]

  //     readAll()
  //   }

  def streaming5() =
    makeDOM { dom => ts =>
      dom.open(TABLE())(using ts)
      dom.open(TBODY())
      dom.open(TR())

      loop[dom.Elems[TStart], dom.Elems[ENil]] {
        ifC[dom.Elems[TStart], dom.Elems[TStart], dom.Elems[ENil]](true) {
          addTwoTC(dom, ???)
          newTableRow(dom)
        } {
          dom.close(TR())
          dom.close(TBODY())
          dom.close(TABLE())
        }
      }
    }

  def streaming6() =
    makeDOM { dom => ts =>
      dom.open(TABLE())(using ts)
      dom.open(TBODY())
      dom.open(TR())

      type TStart = TR :: TBODY :: TABLE :: ENil

      def recur(dom: DOM): dom.Elems[TStart] ?=!>? dom.Elems[ENil] =
        if ??? then
          addTwoTC(dom, ???)
          newTableRow(dom)
          recur(dom)
        else
          dom.close(TR())
          dom.close(TBODY())
          dom.close(TABLE())

      recur(dom)
   }

  def streaming7() =
    makeDOM { dom => ts =>
      dom.open(TABLE())(using ts)
      dom.open(TBODY())
      dom.open(TR())

      type TStart = TR :: TBODY :: TABLE :: ENil

      def recur(dom: DOM): dom.Elems[TStart] ?=!>? dom.Elems[TStart] =
        if ??? then
          addTwoTC(dom, ???)
          newTableRow(dom)
          recur(dom)
        else noChange[dom.Elems[TStart]]

      recur(dom)
      dom.close(TR())
      dom.close(TBODY())
      dom.close(TABLE())
   }

  // def streaming4() =
  //   makeDOM { dom => ts =>
  //     dom.open(TABLE())(using ts)
  //     dom.open(TBODY())
  //     dom.open(TR())

  //     val response = await(Fetch.fetch("/api/logs/stream"))
  //     val reader = response.body.getReader()

  //     def readNext(): (dom.Elems[TR :: TBODY :: TABLE :: ENil]) ?=!>? (dom.Elems[TR :: TBODY :: TABLE :: ENil]) =
  //       val chunk = reader.read()
  //       if (!chunk.done) then
  //         val line = chunk.value
  //         addTwoTC(dom, (line.timeStamp, line.message))
  //         newTableRow(dom)
  //         readNext()

  //     readNext()

  //     dom.close(TR())
  //     dom.close(TBODY())
  //     dom.close(TABLE())
  //   }

  // def test2() =
  //   makeDOM { tree => ts =>
  //     implicit val a = ts
  //     tree.open(HTML()) // fail
  //   }