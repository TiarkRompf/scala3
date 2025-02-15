import scala.annotation.Annotation
// import language.experimental.captureChecking

class eff extends Annotation

object Main:

  def open(): Unit @eff = ()

  def write(s: String): Unit @eff = ()

  def close(): Unit @eff = ()

  def id(x: Int) = x

  def idEff(x: Int): Int @eff = {
    open()
    x
  }

  def eint(): Int @eff = {
    open()
    5
  }

  def argEff(): Unit @eff = {
    id(eint())
    val y = 40
  }

  def retHO(): (Int => Int) @eff = {
    open()
    id
  }

  def appHO(): Int @eff = {
    retHO()(5)
    2
  }

  def higherorder(f: Int => Int @eff): Int @eff = {
    f(7)
  }

  def higherorder_pure(f: Int => Int) = {
    f(7)
  }

  def useHO(): Unit @eff = {
    higherorder(idEff)
    val x = 32
  }

  def useHO_pure() = {
    higherorder_pure(id)
  }

// def if(b: Boolean) = {
//   if (b) {
//     open()
//   }
//   else {
//     val x = 29
//   }
// }
