package typestate

import language.experimental.captureChecking
import caps.*
import scala.annotation

class kill(xs: Any*) extends annotation.StaticAnnotation
object FUN

trait Sigma {
  type A
  type B
  val a: A
  val b: B
}
type `Pair`[A1, B1] = Sigma { type A = A1; type B = B1 }

trait Lock:
  type isHeld // lock is locked, usable
  type isReleased // lock is unlocked, unusable

object Lock:
  extension (lock: Lock)
    def lock()(using c: lock.isReleased^): (Unit `Pair` (lock.isHeld^)) @kill(c) = ???
    def release()(using c: lock.isHeld^): (Unit `Pair` (lock.isReleased^)) @kill(c) = ???

class Table(n: Int) extends Lock:
  private val table: Array[Array[Double]] = new Array[Array[Double]](n)
  class Row(m: Int) extends Lock:
    private val row: Array[Double] = table(m)

object Table:
  def apply(n: Int): Sigma { type A = Table; type B = a.isReleased^ } =
    val table = new Table(n):
      type isReleased = Unit
      type isHeld = Unit
    new Sigma:
      type A = Table
      type B = a.isReleased^
      val a: table.type = table
      val b: a.isReleased^ = ()
  end apply

  extension (table: Table)
    def compute_and_get_row(n: Int)(using c: table.isHeld^):
      Sigma { type A = table.Row; type B = a.isReleased^ } =
      val row = new table.Row(n):
        type isHeld = Unit
        type isReleased = Unit
      new Sigma {
        type A = table.Row;
        type B = a.isReleased^
        val a: row.type = row
        val b: a.isReleased^ = ()
      }

    def compute_on_row(row: table.Row)(using c: row.isHeld^): Double = ???

object Main:
  import Lock.*
  import Table.*

  def example1() =
    val table = Table(40)
    table.lock()
    val row = table.compute_and_get_row(10)
    row.lock()
    val data = table.compute_on_row(row)
    row.release()
    table.release()
    data

  def example2() =
    val table = Table(40)
    table.lock()
    val row = table.compute_and_get_row(10)
    row.lock()
    val data = table.compute_on_row(row)
    table.release() // release table first!
    row.release()
    data





