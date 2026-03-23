import language.experimental.captureChecking
import caps.*
import scala.concurrent.{ Future, ExecutionContext }
import ExecutionContext.Implicits.global
import typestate.*
import scala.annotation.tailrec
import scala.compiletime.ops.int.S

package Channel:
  trait Local
  class Send[B, T <: Local] extends Local
  class Recv[B, T <: Local] extends Local
  class Select[L <: Local, T <: Local] extends Local
  class Branch[L <: Local, T <: Local] extends Local
  class Rec[T <: Local] extends Local
  class Var[N <: Int] extends Local
  class End extends Local

  type Dual[T <: Local] <: Local = T match
    case Send[b, t] => Recv[b, Dual[t]]
    case Recv[b, t] => Send[b, Dual[t]]
    case Select[l, r] => Branch[Dual[l], Dual[r]]
    case Branch[l, r] => Select[Dual[l], Dual[r]]
    case Rec[t] => Rec[Dual[t]]
    case Var[n] => Var[n]
    case End => End

  trait PList
  class PNil extends PList
  class ::[T <: Local, L <: PList] extends PList

  class Chan private[Channel]():
    type PCap[E <: PList, T <: Local]
    private var _isOpen = true
    def isOpen = _isOpen
    private[Channel] def isOpen_=(b : Boolean) = _isOpen = b

  object Chan:
    def apply[T <: Local]():
      ( Sigma { type A = Chan; type B = a.PCap[PNil, T]^ },
        Sigma { type A = Chan; type B = a.PCap[PNil, Dual[T]]^ }) =
      val c1 = new Chan
      val c2 = new Chan
      ( new Sigma:
          type A = Chan
          type B = a.PCap[PNil, T]^
          val a: c1.type = c1
          val b: c1.PCap[PNil, T]^ = ().asInstanceOf[c1.PCap[PNil, T]],
        new Sigma:
          type A = Chan
          type B = a.PCap[PNil, Dual[T]]^
          val a: c2.type = c2
          val b: c2.PCap[PNil, Dual[T]]^ = ().asInstanceOf[c2.PCap[PNil, Dual[T]]]
      )

  extension (chan: Chan)
    def recPush[E <: PList, T <: Local](): chan.PCap[E, Rec[T]] ?=!>? chan.PCap[T :: E, T] =
      Sigma((), ().asInstanceOf[chan.PCap[T :: E, T]])

    def recTop[E <: PList, T <: Local](): chan.PCap[T :: E, Var[0]] ?=!>? chan.PCap[T :: E, T] =
      Sigma((), ().asInstanceOf[chan.PCap[T :: E, T]])

    def recPop[E <: PList, T <: Local, N <: Int](): chan.PCap[T :: E, Var[S[N]]] ?=!>? chan.PCap[E, Var[N]] =
      Sigma((), ().asInstanceOf[chan.PCap[E, Var[N]]])

    def send[B, E <: PList, T <: Local](x: B): chan.PCap[E, Send[B, T]] ?=!>? chan.PCap[E, T] =
      Sigma((), ().asInstanceOf[chan.PCap[E, T]])

    def recv[B, E <: PList, T <: Local](): (chan.PCap[E, Recv[B, T]]^) ?=!> ((chan.PCap[E, T]^) ?<= B) =
      Sigma(???, ().asInstanceOf[chan.PCap[E, T]])
      // new Sigma:
      //   type A = T
      //   type B = chan.PCap[E, P]^
      //   val a: T = ???
      //   val b: chan.PCap[E, P]^ = ().asInstanceOf[chan.PCap[E, P]]

    def left[E <: PList, L <: Local, R <: Local](): chan.PCap[E, Select[L, R]] ?=!>? chan.PCap[E, L] =
      Sigma((), ().asInstanceOf[chan.PCap[E, L]])

    def right[E <: PList, L <: Local, R <: Local](): chan.PCap[E, Select[L, R]] ?=!>? chan.PCap[E, R] =
      Sigma((), ().asInstanceOf[chan.PCap[E, R]])

    def branch[E <: PList, L <: Local, R <: Local, F](using c: chan.PCap[E, Branch[L, R]]^)
      (l: (chan.PCap[E, L]^) ?=!> F)(r: (chan.PCap[E, R]^) ?=!> F): F @kill(c) =
      if ??? then
        l(using ().asInstanceOf[chan.PCap[E, L]])
      else
        r(using ().asInstanceOf[chan.PCap[E, R]])

    def close[E <: PList](): (chan.PCap[E, End]^) ?=!> Unit =
      chan.isOpen = false
end Channel

import Channel.*

type EchoSInner = Recv[String, Branch[Var[0], End]]
type EchoServer = Rec[EchoSInner]
type EchoCInner = Dual[EchoSInner]
type EchoClient = Dual[EchoServer]

def cFuture[T](body: => T @kill(FUN)): Future[T] =
  Future { body.asInstanceOf[T] }

object EchoServer:
  def apply(chan: Chan): (chan.PCap[PNil, EchoServer]^) ?=!> Unit = // chan.PCap[PNil, EchoServer] ?=!> Unit
    chan.recPush()

    def recur(chan: Chan): (chan.PCap[EchoSInner :: PNil, EchoSInner]^) ?=!> Unit =
      println(chan.recv())
      chan.branch {
        chan.recTop()
        recur(chan)
      } {
        chan.close()
      }
    recur(chan)

object EchoClient:
  def readLine(): String = ???

  def apply(chan: Chan): (chan.PCap[PNil, EchoClient]^) ?=!> Unit =
    chan.recPush()

    def recur(chan: Chan): (chan.PCap[EchoCInner :: PNil, EchoCInner]^) ?=!> Unit =
      val msg = readLine()
      chan.send(msg)
      // chan.send(msg)
      val goAgain = readLine()
      if (goAgain == "") then
        chan.left()
        chan.recTop()
        recur(chan)
      else
        chan.right()
        chan.close()

    recur(chan)

type AtmDeposit = Recv[Int, Send[Int, Var[0]]]
type AtmWithdraw = Recv[Int, Select[Var[0], Var[0]]]
type AtmInner = Branch[AtmDeposit, Branch[AtmWithdraw, End]]
type Atm = Recv[String, Select[Rec[AtmInner], End]]

type AtmClientInner = Dual[AtmInner]
type AtmClient = Dual[Atm]

object ATM:
  def approved(id: String): Boolean = ???
  def updateBalance(amt: Int): Int = ???
  def getBalance(id: String): Int = ???

  def apply(chan: Chan): (chan.PCap[PNil, Atm]^) ?=!> Unit =
    val id = chan.recv()
    if !approved(id) then
      chan.right()
      chan.close()
    else
      chan.left()
      chan.recPush()

      def atmInner(chan: Chan): (chan.PCap[AtmInner :: PNil, AtmInner]^) ?=!> Unit =
        chan.branch {
          val amt = chan.recv()
          // update balance
          val newBalance = updateBalance(amt)
          chan.send(newBalance)
          chan.recTop()
          atmInner(chan)
        } {
          chan.branch {
            val amt = chan.recv()
            if getBalance(id) >= amt then
              chan.left()
              chan.recTop()
              atmInner(chan)
              ()
            else
              chan.right()
              chan.recTop()
              atmInner(chan)
              ()
          } {
            chan.close()
          }
        }
      atmInner(chan)

object AtmClient:
  def withdraw(chan: Chan): (chan.PCap[PNil, AtmClient]^) ?=!> Unit =
    val id: String = ???
    chan.send(id)
    chan.branch {
      chan.recPush()
      chan.right()
      chan.left()
      chan.send(200)
      chan.branch {
        println("Withdraw succeeded!")
        chan.recTop()
        chan.right()
        chan.right()
        chan.close()
      } {
        println("Overdraft!")
        chan.recTop()
        chan.left()
        chan.send(50)
        chan.recv()
        chan.recTop()
        chan.right()
        chan.right()
        chan.close()
      }
    } {
      chan.close()
    }

object Main:
  def echotest() =
    val (serverChan, clientChan) = Chan[EchoServer]()
    cFuture {
      EchoServer(serverChan)
    }
    cFuture {
      EchoClient(clientChan)
    }
