package typestate

import language.experimental.captureChecking
import caps.*
import scala.annotation
import scala.compiletime.ops.int.*

class kill(xs: Any*) extends annotation.StaticAnnotation
object FUN

trait Lock:
  type isHeld // lock is locked, usable
  type isReleased // lock is unlocked, unusable

object Lock:
  def lock(lock: Lock, c: lock.isReleased^): (lock.isHeld^) @kill(c) = ???
  def release(lock: Lock, c: lock.isHeld^): (lock.isReleased^) @kill(c) = ???

class LockPair[L <: Lock](val x: L)(val c: x.isReleased^)

class Table(n: Int) extends Lock:
  private val table: Array[Array[Double]] = new Array[Array[Double]](n)
  class Row(m: Int) extends Lock:
    private val row: Array[Double] = table(m)

object Table:
  def compute_and_get_row(table: Table, n: Int, c: table.isHeld^): LockPair[table.Row] =
    val row = new table.Row(n):
      type isHeld = Int
      type isReleased = Int
    new LockPair(row)(0)

  def compute_on_row(table: Table, row: table.Row, c: row.isHeld^): Double = ???

object Main:
  import Lock.*
  import Table.*

  def example1(table: Table, c: table.isReleased^): Double @kill(c) =
    val lock1 = lock(table, c)

    val rowAndLock = compute_and_get_row(table, 5, lock1)

    val lock2 = lock(rowAndLock.x, rowAndLock.c)
    val data = compute_on_row(table, rowAndLock.x, lock2)

    release(rowAndLock.x, lock2)
    release(table, lock1)
    data

  def example2(table: Table, c: table.isReleased^): Double @kill(c) =
    val lock1 = lock(table, c)

    val row5 = compute_and_get_row(table, 5, lock1)
    val lock2 = lock(row5.x, row5.c)
    val row6 = compute_and_get_row(table, 6, lock1)

    release(table, lock1)
    val data = compute_on_row(table, row5.x, lock2)
    release(row5.x, lock2)
    data





