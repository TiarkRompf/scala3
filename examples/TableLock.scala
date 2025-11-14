import language.experimental.captureChecking
import caps.*
import typestate.*

package LockableTable:
  trait Lock:
    type IsHeld // lock is locked, usable
    type IsReleased // lock is unlocked, unusable

  class Table private (n: Int) extends Lock:
    private val table: Array[Array[Double]] = new Array[Array[Double]](n)
    class Row private[LockableTable] (m: Int) extends Lock:
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
    def lock(): table.IsReleased ?=!>? table.IsHeld =
      Sigma((), ().asInstanceOf[table.IsHeld])

    def unlock(): table.IsHeld ?=!>? table.IsReleased =
      Sigma((), ().asInstanceOf[table.IsReleased])

    def locateRow(n: Int): table.IsHeld^ ?=> Sigma { type A = table.Row; type B = a.IsReleased^ } =
      val row = new table.Row(n):
        type IsHeld = Unit
        type IsReleased = Unit
      new Sigma:
        type A = table.Row;
        type B = a.IsReleased^
        val a: row.type = row
        val b: a.IsReleased^ = ()

    def lockRow(row: table.Row): table.IsHeld^ ?=> row.IsReleased ?=!>? row.IsHeld =
      Sigma((), ().asInstanceOf[row.IsHeld])

  extension (row: Table#Row) def unlock(): row.IsHeld ?=!>? row.IsReleased =
    Sigma((), ().asInstanceOf[row.IsReleased])

  def computeOnRow(row: Table#Row): row.IsHeld^ ?=> Double = 5.0

object Main:
  import LockableTable.*

  def example1() =
    val table = Table(40)
    table.lock()
    val row = table.locateRow(5)
    table.lockRow(row)
    val result = computeOnRow(row)
    row.unlock()
    result

  def example2() =
    val table = Table(40)
    table.lock()
    val row = table.locateRow(5)
    table.lockRow(row)
    table.unlock() // unlock table first
    val result = computeOnRow(row)
    row.unlock()
    // table.lockRow(row) // error
    result

  def example3() =
    val table1 = Table(30)
    val table2 = Table(50)

    table1.lock()
    table2.lock()
    val row1 = table1.locateRow(10)
    table1.lockRow(row1)
    val row2 = table2.locateRow(40)
    // table2.lockRow(row1) // error
    table2.lockRow(row2)
    val data = computeOnRow(row2)

  def main(args: Array[String]): Unit =
    example1()
    example2()
    example3()
