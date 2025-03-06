package typestate

import language.experimental.captureChecking
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation

trait Elem(val name: String)
class HTML extends Elem("<html>")
class HEAD extends Elem("<head>")
class TITLE extends Elem("<title>")
class BODY extends Elem("<body>")
class P extends Elem("<p>")

trait List
class Nil extends List
class Cons[E <: Elem, T <: List] extends List

object DOM:
  def open[E <: Elem](e: E, treeState: List): Cons[E, List] = // do not kill the previous treestate?
    0.asInstanceOf[Cons[E, List]]

  def close[E <: Elem](e: E, treeState: Cons[E, List]): List = // kill the previous treestate here
    0.asInstanceOf[List]

  def add[E <: Elem](s: String, e: E, treeState: Cons[E, List]): Unit = ()

  def mkDom(body: Nil => Nil) =
    body(new Nil)

object Main:
  import DOM.*
  def test1() =
    mkDom { nil =>
      val o1 = open(HTML(), nil)
      val o2 = open(HEAD(), o1)
      val o3 = open(TITLE(), o2)

      add("some title", TITLE(), o3)

      val c3 = close(TITLE(), o3)
      val c2 = close(HEAD(), o2)
      val c1 = close(HTML(), o1) // the "Nil" is not preserved so c1 cannot be returned.
      nil // if we dont kill nil at open - can return nil at will within body
    }