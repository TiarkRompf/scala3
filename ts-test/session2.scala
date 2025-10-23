// Based on https://munksgaard.me/papers/laumann-munksgaard-larsen.pdf
// Iteration 2 - each channel has an associated path-dependent capability parameterized by the channel PCapate.
import language.experimental.captureChecking
import caps.*
// import scala.concurrent.{ Future, ExecutionContext }
// import ExecutionContext.Implicits.global
import typestate.*
import scala.annotation.tailrec
import scala.compiletime.ops.int.S

trait PList
class PNil extends PList
class ::[P <: Session, L <: PList] extends PList

class Chan:
  type PCap[E <: PList, P <: Session]
  // type ES[P <: Session] = this.PCap[PNil, P]
  // type withProtocol[E <: PList, P <: Session, T] = (c: this.PCap[E, P]^) ?=> (T) @kill(c)
  // type withEProtocol[P <: Session, T] = (c: this.PCap[PNil, P]^) ?=> (T) @kill(c)
  private var _isOpen = true
  def isOpen = _isOpen

trait Session
class Send[T, P <: Session] extends Session
class Recv[T, P <: Session] extends Session
class Select[L <: Session, R <: Session] extends Session
class Branch[L <: Session, R <: Session] extends Session
class Rec[P <: Session] extends Session
class Var[N <: Int] extends Session
class End extends Session
// class Delegate[Chan <: Session, P <: Session] extends Session
// class RecvChan[Chan <: Session, P <: Session]

type Dual[P <: Session] <: Session = P match
  case Send[t, p] => Recv[t, Dual[p]]
  case Recv[t, p] => Send[t, Dual[p]]
  case Select[l, r] => Branch[Dual[l], Dual[r]]
  case Branch[l, r] => Select[Dual[l], Dual[r]]
  case Rec[p] => Rec[Dual[p]]
  case Var[n] => Var[n]
  case End => End

object Chan:
  def apply[P <: Session]():
    ( Sigma {type A = Chan; type B = a.PCap[PNil, P]^ },
      Sigma {type A = Chan; type B = a.PCap[PNil, Dual[P]]^ }) =
    val c1 = new Chan
    val c2 = new Chan
    ( new Sigma:
        type A = Chan
        type B = a.PCap[PNil, P]^
        val a: c1.type = c1
        val b: c1.PCap[PNil, P]^ = ().asInstanceOf[c1.PCap[PNil, P]],
      new Sigma:
        type A = Chan
        type B = a.PCap[PNil, Dual[P]]^
        val a: c2.type = c2
        val b: c2.PCap[PNil, Dual[P]]^ = ().asInstanceOf[c2.PCap[PNil, Dual[P]]]
    )

  extension (chan: Chan)
    def recPush[E <: PList, P <: Session](): chan.PCap[E, Rec[P]] ?=!>? chan.PCap[P :: E, P] =
      ???
      // Sigma((), ().asInstanceOf[chan.PCap[P :: E, P]])

    def recTop[E <: PList, P <: Session](): chan.PCap[P :: E, Var[0]] ?=!>? chan.PCap[P :: E, P] =
      ??? // c.asInstanceOf[chan.PCap[(P, E), P]]

    def recPop[E <: PList, P <: Session, N <: Int](): chan.PCap[P :: E, Var[S[N]]] ?=!>? chan.PCap[E, Var[N]] =
      ??? // c.asInstanceOf[chan.PCap[E, Var[N]]]

    def send[T, E <: PList, P <: Session](x: T): chan.PCap[E, Send[T, P]] ?=!>? chan.PCap[E, P] =
      ??? // c.asInstanceOf[chan.PCap[E, P]]

    def recv[T, E <: PList, P <: Session](): (chan.PCap[E, Recv[T, P]]^) ?=!> ((chan.PCap[E, P]^) ?<= T) =
      new Sigma:
        type A = T
        type B = chan.PCap[E, P]^
        val a: T = ???
        val b: chan.PCap[E, P]^ = ().asInstanceOf[chan.PCap[E, P]]

    def left[E <: PList, L <: Session, R <: Session](): chan.PCap[E, Select[L, R]] ?=!>? chan.PCap[E, L] =
      ??? // c.asInstanceOf[chan.PCap[E, L]]

    def right[E <: PList, L <: Session, R <: Session](): chan.PCap[E, Select[L, R]] ?=!>? chan.PCap[E, R] =
      ??? // c.asInstanceOf[chan.PCap[E, R]]

    def branch[E <: PList, L <: Session, R <: Session, T](using c: chan.PCap[E, Branch[L, R]]^)
      (l: (chan.PCap[E, L]^) ?=!> T)(r: (chan.PCap[E, R]^) ?=!> T): (T) @kill(c) =
      // if ??? then
      //   l(using c.asInstanceOf[chan.PCap[E, L]])
      // else
      //   r(using c.asInstanceOf[chan.PCap[E, R]])
      ???

    // def branch2[E <: PList, L <: Session, R <: Session](using c: chan.PCap[E, Branch[L, R]]^)
    //   [T](l: chan.PCap[E, L]^ ?=> T^)(r: chan.PCap[E, R]^ ?=> T^): (T^) @kill(c) =
    //   if ??? then
    //     l(using c.asInstanceOf[chan.PCap[E, L]])
    //   else
    //     r(using c.asInstanceOf[chan.PCap[E, R]])

    def close[E <: PList](): (chan.PCap[E, End]^) ?=> Unit =
      chan._isOpen = false

    // def loop[T, U^](using c: T^{U})(cond: => Boolean)
    // (body : k: T^{U} ?=> Sigma { type A = Unit; type B = T^{U}} @kill(k)): Unit @kill(c)

    // def loop[E <: PList, P <: Session](using c: chan.PCap[E, P]^)(cond: => Boolean)
    // (body: (k: chan.PCap[E, P]^) ?=> Option[chan.PCap[E, P]^] @kill(k)): Unit @kill(c) =
    //   if cond then
    //     body(using c) match
    //       case Some(c) =>
    //         chan.loop(using c)(cond)(body)
    //       case None =>

type EchoSInner = Recv[String, Branch[Var[0], End]]
type EchoServer = Rec[EchoSInner]
type EchoCInner = Dual[EchoSInner]
type EchoClient = Dual[EchoServer]
type Simple = Send[Int, Recv[String, End]]

object EchoServer:
  def apply(chan: Chan): (chan.PCap[PNil, EchoServer]^) ?=!> Unit = // chan.PCap[PNil, EchoServer] ?=!> Unit
    chan.recPush()

    def recur(chan: Chan): (chan.PCap[EchoSInner :: PNil, EchoSInner]^) ?=!> Unit =
      val msg = chan.recv()
      println(msg)
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

// object Main:
//   def echotest() =
//     val (serverChan, clientChan) = Chan[EchoServer]()
//     Future {
//       EchoServer(serverChan)
//     }
//     Future {
//       EchoClient(clientChan)
//     }
