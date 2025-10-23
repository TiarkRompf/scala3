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

trait TList
class TNil extends TList
class ::[E <: Elem, L <: TList] extends TList

class DOM:
  type Elems[T <: TList]

object DOM:
  extension (tree: DOM)
    // find way to avoid elem term parameter?
    def open[E <: Elem, L <: TList](elem: E): tree.Elems[L] ?=!>? tree.Elems[E :: L] =
      Sigma((), ().asInstanceOf[tree.Elems[E :: L]])

    def close[E <: Elem, L <: TList](elem: E): tree.Elems[E :: L] ?=!>? tree.Elems[L] =
      Sigma((), ().asInstanceOf[tree.Elems[L]])

    def addText[E <: Elem, L <: TList](elem: E, s: String): tree.Elems[E :: L]^ ?=> Unit =
      ()

  def makeDOM(body: (tree: DOM) => (tree.Elems[TNil]^) =!> (tree.Elems[TNil]^) ?<= Unit): Unit =
    val dom = new DOM:
      type Elems[TNil] = Unit
    body(dom)(())

  // def makeDOM(body: (tree: DOM) => (ts: tree.Elems[TNil]^) =>
  //     (Sigma { type A = Unit; type B = tree.Elems[TNil]^{ts, cap} }^{ts, cap}) @kill(ts)): Unit =
  //   val dom = new DOM:
  //     type Elems[TNil] = Unit
  //   body(dom)(())

object Main:
  import DOM.*

  def newTableRow[L <: TList](tree: DOM): tree.Elems[TR :: L] ?=!>? tree.Elems[TR :: L] =
    tree.close(TR())
    tree.open(TR())

  def addTwoTC[L <: TList](tree: DOM, fst: String, scd: String): tree.Elems[TR :: L] ?=!>? tree.Elems[TR :: L] =
    tree.open(TD())
    tree.addText(TD(), fst)
    tree.close(TD())
    tree.open(TD())
    tree.addText(TD(), scd)
    tree.close(TD())

  def test2() =
    makeDOM { tree => ts => // ts: tree.Elems[TNil]
      tree.open(TABLE())(using ts) // tree.Elems[TABLE :: TNil]
      tree.open(TR()) // tree.Elems[TABLE :: TNil] >> tree.Elems[TR :: TABLE :: TNil]
      addTwoTC(tree, "cell1", "cell2") // stays tree.Elems[TR :: TABLE :: TNil]
      newTableRow(tree) // stays tree.Elems[TR :: TABLE :: TNil]
      addTwoTC(tree, "cell3", "cell4")
      tree.close(TR()) // tree.Elems[TR :: TABLE :: TNil] >> tree.Elems[TABLE :: TNil]
      tree.close(TABLE()) // tree.Elems[TABLE :: TNil] >> tree.Elems[TNil]
    }

  def test1() =
    makeDOM { tree => ts =>
      tree.open(HTML())(using ts)
      tree.open(HEAD())
      tree.addText(HEAD(), "a")
      tree.open(P()) // tree.Elems[P :: ]
      tree.open(P()) // tree.Elems[P :: ]
      tree.close(P())
      tree.close(P())
      // tree.close(P()) // fail
      tree.close(HEAD())
      tree.close(HTML())
    }

  // def test2() =
  //   mkDom { tree => ts =>
  //     implicit val a = ts
  //     tree.open(HTML()) // fail
  //   }