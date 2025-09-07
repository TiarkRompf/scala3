import language.experimental.captureChecking
import caps.cap
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
    def open[E <: Elem, L <: TList](elem: E): tree.Nodes[L] >> tree.Nodes[E :: L] =
      ???

    def close[E <: Elem, L <: TList](elem: E): tree.Nodes[E :: L] >> tree.Nodes[L] =
      ???

    def add[E <: Elem, L <: TList](elem: E, s: String)(using ts: tree.Nodes[E :: L]^): Unit =
      ???

  def mkDom(body: (tree: DOM) => (ts: tree.Nodes[TNil]^) =>
      (Sigma { type A = Unit; type B = tree.Nodes[TNil]^{ts, cap} }^{ts, cap}) @kill(ts)): Unit =
    val dom = new DOM:
      type Nodes[TNil] = Unit
    body(dom)(())

object Main:
  import DOM.*

  def test1() =
    mkDom { tree => ts =>
      tree.open(HTML())(using ts)
      tree.open(HEAD())
      tree.add(HEAD(), "a")
      tree.open(P())
      tree.open(P())
      tree.close(P())
      tree.close(P())
      tree.close(HEAD())
      tree.close(HTML())
    }

  // def test2() =
  //   mkDom { tree => ts =>
  //     implicit val a = ts
  //     tree.open(HTML()) // fail
  //   }