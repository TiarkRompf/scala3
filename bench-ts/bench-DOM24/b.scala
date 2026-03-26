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
  class ::[E <: Elem, L <: EList] extends EList

  trait Sigma:
    type A
    type B
    val a: A
    val b: B

  def Sigma[A1, B1](a1: A1, b1: B1): Sigma { type A = A1; type B = B1 } =
    new Sigma:
      type A = A1
      type B = B1
      val a: A1 = a1
      val b: B1 = b1

  infix type ?=>?[S1, S2] = S1 ?=> Sigma { type A = Unit; type B = S2 }

  infix type ?<=[B1, A1] = Sigma { type A = A1; type B = B1 }

  class DOM private[ManualDOM]():
    type Elems[T <: EList]

  extension (tree: DOM)
    def open[E <: Elem, L <: EList](elem: E): tree.Elems[L] ?=>? tree.Elems[E :: L] =
      Sigma((), ().asInstanceOf[tree.Elems[E :: L]])

    def close[E <: Elem, L <: EList](elem: E): tree.Elems[E :: L] ?=>? tree.Elems[L] =
      Sigma((), ().asInstanceOf[tree.Elems[L]])

    def text[E <: Elem, L <: EList](elem: E, s: String): tree.Elems[E :: L] ?=> Unit =
      ()

  def makeDOM(body: (tree: DOM) => tree.Elems[ENil] => (tree.Elems[ENil] ?<= Unit)): Unit =
    val dom = new DOM:
      type Elems[ENil] = Unit
    body(dom)(())

  object DOM:
    def apply(): Sigma { type A = DOM; type B = a.Elems[ENil] } =
      val dom = new DOM:
        type Elems[ENil] = Unit
      new Sigma {
        type A = DOM
        type B = a.Elems[ENil]
        val a: dom.type = dom
        val b: dom.Elems[ENil] = ()
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

object Main:
  import ManualDOM.*

  def nextTR[L <: EList](tree: DOM): tree.Elems[TR :: L] ?=>? tree.Elems[TR :: L] =
    val s1 = tree.close(TR())
    {

      implicit val s1_cap = s1.b
      tree.open(TR())
    }

  def twoCells[L <: EList](tree: DOM, fst: String, scd: String): tree.Elems[TR :: L] ?=>? tree.Elems[TR :: L] =
    val s1 = tree.open(TD())
    {

      implicit val s1_cap = s1.b
      tree.text(TD(), fst)
      val s2 = tree.close(TD())
      {

      implicit val s2_cap = s2.b
      val s3 = tree.open(TD())
      {

      implicit val s3_cap = s3.b;
      tree.text(TD(), scd)
      tree.close(TD())
      }}
    }

  def main1() =
    makeDOM { dom => ts =>
      val s1 = dom.open(TABLE())(using ts)
      {

      implicit val s1_cap = s1.b
      val s2 = dom.open(TBODY())
      {

      implicit val s2_cap = s2.b
      val s10 = dom.open(TR())
      {

      implicit val s10_cap = s10.b
      val response = await(fetch("/api/logs").toFuture)
      val reader = response.body.getReader()

      type TStart = TR :: TBODY :: TABLE :: ENil

      def readAll(dom: DOM): dom.Elems[TStart] ?=>? dom.Elems[ENil] = {
        val chunk = await(reader.read().toFuture)
        if !chunk.isDone then
          val line = chunk.value
          val s3 = twoCells(dom, line.timeStamp, line.message)
          {

          implicit val s3_cap = s3.b
          val s4 = nextTR(dom)
          {

          implicit val s4_cap = s4.b
          readAll(dom)
          }
          }
        else
          val s3 = dom.close(TR())
          {

          implicit val s3_cap = s3.b
          val s4 = dom.close(TBODY())
          {

          implicit val s4_cap = s4.b
          dom.close(TABLE())
          }
          }
      }
      readAll(dom)
      }}}
    }

  def test1() =
    makeDOM { tree => ts =>
      val s1 = tree.open(HTML())(using ts)
      {

      implicit val s1_cap = s1.b
      val s2 = tree.open(HEAD())
      {

      implicit val s2_cap = s2.b
      tree.text(HEAD(), "a")
      val s3 = tree.open(P())
      {

      val s4 = tree.open(P())
      {

      implicit val s4_cap = s4.b
      val s5 = tree.close(P())
      {

      implicit val s5_cap = s5.b
      val s6 = tree.close(P())
      {

      implicit val s6_cap = s6.b
      val s7 = tree.close(HEAD())
      {

      implicit val s7_cap = s7.b
      tree.close(HTML())
      }}}}}}}
    }

    def test2() =
      makeDOM { tree => ts =>
        val s0 = tree.open(HTML())(using ts)
        {
        implicit val s0_cap = s0.b
        val s1 = tree.open(HEAD())
        {
        implicit val s1_cap = s1.b
        val s2 = tree.open(TITLE())
        {
        implicit val s2_cap = s2.b
        val s3 = tree.open(BODY())
        {
        implicit val s3_cap = s3.b
        val s4 = tree.open(P())
        {
        implicit val s4_cap = s4.b
        val s5 = tree.open(HEAD())
        {
        implicit val s5_cap = s5.b
        val s6 = tree.open(TITLE())
        {
        implicit val s6_cap = s6.b
        val s7 = tree.open(BODY())
        {
        implicit val s7_cap = s7.b
        val s8 = tree.open(P())
        {
        implicit val s8_cap = s8.b
        val s9 = tree.open(HEAD())
        {
        implicit val s9_cap = s9.b
        val s10 = tree.open(TITLE())
        {
        implicit val s10_cap = s10.b
        val s11 = tree.open(BODY())
        {
        implicit val s11_cap = s11.b
        val s12 = tree.open(P())
        {
        implicit val s12_cap = s12.b
        val s13 = tree.open(HEAD())
        {
        implicit val s13_cap = s13.b
        val s14 = tree.open(TITLE())
        {
        implicit val s14_cap = s14.b
        val s15 = tree.open(BODY())
        {
        implicit val s15_cap = s15.b
        val s16 = tree.open(P())
        {
        implicit val s16_cap = s16.b
        val s17 = tree.open(HEAD())
        {
        implicit val s17_cap = s17.b
        val s18 = tree.open(TITLE())
        {
        implicit val s18_cap = s18.b
        val s19 = tree.open(BODY())
        {
        implicit val s19_cap = s19.b
        val s20 = tree.open(P())
        {
        implicit val s20_cap = s20.b
        val s21 = tree.open(HEAD())
        {
        implicit val s21_cap = s21.b
        val s22 = tree.open(TITLE())
        {
        implicit val s22_cap = s22.b
        val s23 = tree.open(BODY())
        {
        implicit val s23_cap = s23.b
        val s24 = tree.open(P())
        {
        implicit val s24_cap = s24.b
        val s25 = tree.close(P())
        {
        implicit val s25_cap = s25.b
        val s26 = tree.close(BODY())
        {
        implicit val s26_cap = s26.b
        val s27 = tree.close(TITLE())
        {
        implicit val s27_cap = s27.b
        val s28 = tree.close(HEAD())
        {
        implicit val s28_cap = s28.b
        val s29 = tree.close(P())
        {
        implicit val s29_cap = s29.b
        val s30 = tree.close(BODY())
        {
        implicit val s30_cap = s30.b
        val s31 = tree.close(TITLE())
        {
        implicit val s31_cap = s31.b
        val s32 = tree.close(HEAD())
        {
        implicit val s32_cap = s32.b
        val s33 = tree.close(P())
        {
        implicit val s33_cap = s33.b
        val s34 = tree.close(BODY())
        {
        implicit val s34_cap = s34.b
        val s35 = tree.close(TITLE())
        {
        implicit val s35_cap = s35.b
        val s36 = tree.close(HEAD())
        {
        implicit val s36_cap = s36.b
        val s37 = tree.close(P())
        {
        implicit val s37_cap = s37.b
        val s38 = tree.close(BODY())
        {
        implicit val s38_cap = s38.b
        val s39 = tree.close(TITLE())
        {
        implicit val s39_cap = s39.b
        val s40 = tree.close(HEAD())
        {
        implicit val s40_cap = s40.b
        val s41 = tree.close(P())
        {
        implicit val s41_cap = s41.b
        val s42 = tree.close(BODY())
        {
        implicit val s42_cap = s42.b
        val s43 = tree.close(TITLE())
        {
        implicit val s43_cap = s43.b
        val s44 = tree.close(HEAD())
        {
        implicit val s44_cap = s44.b
        val s45 = tree.close(P())
        {
        implicit val s45_cap = s45.b
        val s46 = tree.close(BODY())
        {
        implicit val s46_cap = s46.b
        val s47 = tree.close(TITLE())
        {
        implicit val s47_cap = s47.b
        val s48 = tree.close(HEAD())
        {
        implicit val s48_cap = s48.b
        tree.close(HTML())
        }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
      }

    def test3() =
      makeDOM { tree => ts =>
        val s0 = tree.open(HTML())(using ts)
        {
        implicit val s0_cap = s0.b
        val s1 = tree.open(HEAD())
        {
        implicit val s1_cap = s1.b
        val s2 = tree.open(TITLE())
        {
        implicit val s2_cap = s2.b
        val s3 = tree.open(BODY())
        {
        implicit val s3_cap = s3.b
        val s4 = tree.open(P())
        {
        implicit val s4_cap = s4.b
        val s5 = tree.open(HEAD())
        {
        implicit val s5_cap = s5.b
        val s6 = tree.open(TITLE())
        {
        implicit val s6_cap = s6.b
        val s7 = tree.open(BODY())
        {
        implicit val s7_cap = s7.b
        val s8 = tree.open(P())
        {
        implicit val s8_cap = s8.b
        val s9 = tree.open(HEAD())
        {
        implicit val s9_cap = s9.b
        val s10 = tree.open(TITLE())
        {
        implicit val s10_cap = s10.b
        val s11 = tree.open(BODY())
        {
        implicit val s11_cap = s11.b
        val s12 = tree.open(P())
        {
        implicit val s12_cap = s12.b
        val s13 = tree.open(HEAD())
        {
        implicit val s13_cap = s13.b
        val s14 = tree.open(TITLE())
        {
        implicit val s14_cap = s14.b
        val s15 = tree.open(BODY())
        {
        implicit val s15_cap = s15.b
        val s16 = tree.open(P())
        {
        implicit val s16_cap = s16.b
        val s17 = tree.open(HEAD())
        {
        implicit val s17_cap = s17.b
        val s18 = tree.open(TITLE())
        {
        implicit val s18_cap = s18.b
        val s19 = tree.open(BODY())
        {
        implicit val s19_cap = s19.b
        val s20 = tree.open(P())
        {
        implicit val s20_cap = s20.b
        val s21 = tree.open(HEAD())
        {
        implicit val s21_cap = s21.b
        val s22 = tree.open(TITLE())
        {
        implicit val s22_cap = s22.b
        val s23 = tree.open(BODY())
        {
        implicit val s23_cap = s23.b
        val s24 = tree.open(P())
        {
        implicit val s24_cap = s24.b
        val s25 = tree.close(P())
        {
        implicit val s25_cap = s25.b
        val s26 = tree.close(BODY())
        {
        implicit val s26_cap = s26.b
        val s27 = tree.close(TITLE())
        {
        implicit val s27_cap = s27.b
        val s28 = tree.close(HEAD())
        {
        implicit val s28_cap = s28.b
        val s29 = tree.close(P())
        {
        implicit val s29_cap = s29.b
        val s30 = tree.close(BODY())
        {
        implicit val s30_cap = s30.b
        val s31 = tree.close(TITLE())
        {
        implicit val s31_cap = s31.b
        val s32 = tree.close(HEAD())
        {
        implicit val s32_cap = s32.b
        val s33 = tree.close(P())
        {
        implicit val s33_cap = s33.b
        val s34 = tree.close(BODY())
        {
        implicit val s34_cap = s34.b
        val s35 = tree.close(TITLE())
        {
        implicit val s35_cap = s35.b
        val s36 = tree.close(HEAD())
        {
        implicit val s36_cap = s36.b
        val s37 = tree.close(P())
        {
        implicit val s37_cap = s37.b
        val s38 = tree.close(BODY())
        {
        implicit val s38_cap = s38.b
        val s39 = tree.close(TITLE())
        {
        implicit val s39_cap = s39.b
        val s40 = tree.close(HEAD())
        {
        implicit val s40_cap = s40.b
        val s41 = tree.close(P())
        {
        implicit val s41_cap = s41.b
        val s42 = tree.close(BODY())
        {
        implicit val s42_cap = s42.b
        val s43 = tree.close(TITLE())
        {
        implicit val s43_cap = s43.b
        val s44 = tree.close(HEAD())
        {
        implicit val s44_cap = s44.b
        val s45 = tree.close(P())
        {
        implicit val s45_cap = s45.b
        val s46 = tree.close(BODY())
        {
        implicit val s46_cap = s46.b
        val s47 = tree.close(TITLE())
        {
        implicit val s47_cap = s47.b
        val s48 = tree.close(HEAD())
        {
        implicit val s48_cap = s48.b
        tree.close(HTML())
        }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
      }