import language.experimental.captureChecking
import typestate.*

trait Elem(val name: String)
class HTML() extends Elem("<html>")
class HEAD() extends Elem("<head>")
class TITLE() extends Elem("<title>")
class BODY() extends Elem("<body>")
class P() extends Elem("<p>")

trait TList
class TNil extends TList
class ::[E <: Elem, L <: TList] extends TList

class DOM:
  type Nodes[T <: TList]

object DOM:
  extension (tree: DOM)
    // find way to avoid elem term parameter?
    def open[E <: Elem, L <: TList](elem: E)(using ts: tree.Nodes[L]^): (IBox[tree.Nodes[E :: L]^]) @kill(ts) =
      ???

    def close[E <: Elem, L <: TList](elem: E)(using ts: tree.Nodes[E :: L]^): (IBox[tree.Nodes[L]^]) @kill(ts) =
      ???

    def add[E <: Elem, L <: TList](elem: E, s: String)(using ts: tree.Nodes[E :: L]^): Unit =
      ???

  def mkDom(body: (tree: DOM) => (ts: tree.Nodes[TNil]^) => (`IBox`[tree.Nodes[TNil]^]^) @kill(ts)): Unit =
    val dom = new DOM:
      type Nodes[TNil] = Unit
    body(dom)(())

object Main:
  import DOM.*

  def test1() =
    mkDom { tree => ts =>
      implicit val a = ts
      tree.open(HTML())
      tree.open(HEAD())
      tree.add(HEAD(), "a")
      tree.open(P())
      tree.open(P())
      tree.close(P())
      tree.close(P())
      tree.close(HEAD())
      tree.close(HTML())
    }