// package typestate

import language.experimental.captureChecking
import caps.*
import typestate.*

trait Lock:
  type IsHeld // lock is locked, usable
  type IsReleased // lock is unlocked, unusable

object Lock:
  extension (lock: Lock)
    def lock()(using c: lock.IsReleased^): (Unit `Pair` (lock.IsHeld^)) @kill(c) =
      new Sigma:
        type A = Unit
        type B = lock.IsHeld^
        val a = ()
        val b = ().asInstanceOf[lock.IsHeld^]

    def release()(using c: lock.IsHeld^): (Unit `Pair` (lock.IsReleased^)) @kill(c) =
      new Sigma:
        type A = Unit
        type B = lock.IsReleased^
        val a = ()
        val b = ().asInstanceOf[lock.IsReleased^]

class Table(n: Int) extends Lock:
  private val table: Array[Array[Double]] = new Array[Array[Double]](n)
  class Row(m: Int) extends Lock:
    private val row: Array[Double] = table(m)

object Table:
  def apply(n: Int): Sigma { type A = Table; type B = a.IsReleased^ } =
    val table = new Table(n):
      type IsReleased = Unit
      type IsHeld = Unit
    new Sigma:
      type A = Table
      type B = a.IsReleased^
      val a: table.type = table
      val b: a.IsReleased^ = ()
  end apply

  extension (table: Table)
    def compute_and_get_row(n: Int)(using c: table.IsHeld^):
      Sigma { type A = table.Row; type B = a.IsReleased^ } =
      val row = new table.Row(n):
        type IsHeld = Unit
        type IsReleased = Unit
      new Sigma {
        type A = table.Row;
        type B = a.IsReleased^
        val a: row.type = row
        val b: a.IsReleased^ = ()
      }

    def compute_on_row(row: table.Row)(using c: row.IsHeld^): Double = ???

object Main:
  import Lock.*
  import Table.*

  def example1(): Double =
    val table = Table(40)
    table.lock()
    val row = table.compute_and_get_row(10)
    row.lock()
    val data = table.compute_on_row(row)
    row.release()
    // row.release()
    // table.compute_on_row(row)
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





