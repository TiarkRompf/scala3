import language.experimental.captureChecking
import caps.*
import typestate.*

trait Lock:
  type IsHeld // lock is locked, usable
  type IsReleased // lock is unlocked, unusable

// object Lock:
//   extension (lock: Lock)
//     def lock(): lock.IsReleased >> lock.IsHeld =
//       new Sigma:
//         type A = Unit
//         type B = lock.IsHeld^
//         val a = ()
//         val b = ().asInstanceOf[lock.IsHeld^]

//     def unlock(): lock.IsHeld >> lock.IsReleased =
//       new Sigma:
//         type A = Unit
//         type B = lock.IsReleased^
//         val a = ()
//         val b = ().asInstanceOf[lock.IsReleased^]

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
    def lock(): table.IsReleased >> table.IsHeld =
      new Sigma:
        type A = Unit
        type B = table.IsHeld^
        val a = ()
        val b = ().asInstanceOf[table.IsHeld^]

    def unlock(): table.IsHeld >> table.IsReleased =
      new Sigma:
        type A = Unit
        type B = table.IsReleased^
        val a = ()
        val b = ().asInstanceOf[table.IsReleased^]

    def locate_row(n: Int)(using c: table.IsHeld^):
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

    def lock_row(row: table.Row)(using c: table.IsHeld^): row.IsReleased >> row.IsHeld =
      new Sigma:
        type A = Unit
        type B = row.IsHeld^
        val a = ()
        val b = ().asInstanceOf[row.IsHeld^]

    def compute_on_row(row: table.Row)(using c: row.IsHeld^): Double = 5.0

  extension (row: Table#Row)
      def unlock(): row.IsHeld >> row.IsReleased =
        new Sigma:
          type A = Unit
          type B = row.IsReleased^
          val a = ()
          val b = ().asInstanceOf[row.IsReleased^]

object Main:
  import Table.*

  def example1() =
    val table = Table(40)
    table.lock()
    val row = table.locate_row(5)
    table.lock_row(row)
    val result = table.compute_on_row(row)
    row.unlock()
    result

  def example2() =
    val table = Table(40)
    table.lock()
    val row = table.locate_row(5)
    table.lock_row(row)
    table.unlock() // unlock table first
    val result = table.compute_on_row(row)
    row.unlock()
    // table.lock_row(row) // error
    result

  def example3() =
    val table1 = Table(30)
    val table2 = Table(50)

    table1.lock()
    table2.lock()
    val row1 = table1.locate_row(10)
    table1.lock_row(row1)
    val row2 = table2.locate_row(50)
    // val bad = table2.compute_on_row(row1)
    table2.lock_row(row2)
    val data = table2.compute_on_row(row2)

  // def example2() =
  //   val table = Table(40)

