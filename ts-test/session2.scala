// Based on https://munksgaard.me/papers/laumann-munksgaard-larsen.pdf
// Iteration 2 - each channel has an associated path-dependent capability parameterized by the channel CSate.
import language.experimental.captureChecking
import caps.*
import typestate.*
import scala.annotation.tailrec
import scala.compiletime.ops.int.*

trait Session
class Send[T, P <: Session] extends Session
class Recv[T, P <: Session] extends Session
class Select[L <: Session, R <: Session] extends Session
class Branch[L <: Session, R <: Session] extends Session
class Rec[P <: Session] extends Session
class Var[N <: Int] extends Session
class End extends Session

type Dual[P <: Session] <: Session = P match
  case Send[t, p] => Recv[t, Dual[p]]
  case Recv[t, p] => Send[t, Dual[p]]
  case Select[l, r] => Branch[Dual[l], Dual[r]]
  case Branch[l, r] => Select[Dual[l], Dual[r]]
  case Rec[p] => Rec[Dual[p]]
  case Var[n] => Var[n]
  case End => End

class Chan:
  type CS[E <: Tuple, P <: Session] // CS for channel session
  type ES[P <: Session] = this.CS[EmptyTuple, P]
  type withProtocol[E <: Tuple, P <: Session, T] = (c: this.CS[E, P]^) ?=> (T) @kill(c)
  type withEProtocol[P <: Session, T] = (c: this.CS[EmptyTuple, P]^) ?=> (T) @kill(c)
  private var _isOpen = true
  def isOpen = _isOpen

object Chan:
  def apply[P <: Session]():
    ( Sigma {type A = Chan; type B = a.CS[EmptyTuple, P]^ },
      Sigma {type A = Chan; type B = a.CS[EmptyTuple, Dual[P]]^ }) =
    val c1 = new Chan
    val c2 = new Chan
    ( new Sigma:
        type A = Chan
        type B = a.CS[EmptyTuple, P]^
        val a: c1.type = c1
        val b: c1.CS[EmptyTuple, P]^ = ().asInstanceOf[c1.CS[EmptyTuple, P]],
      new Sigma:
        type A = Chan
        type B = a.CS[EmptyTuple, Dual[P]]^
        val a: c2.type = c2
        val b: c2.CS[EmptyTuple, Dual[P]]^ = ().asInstanceOf[c2.CS[EmptyTuple, Dual[P]]]
    )

  extension (chan: Chan)
    def rec_push[E <: Tuple, P <: Session]()(using c: chan.CS[E, Rec[P]]^): (`Pair`[Unit, chan.CS[P *: E, P]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[P *: E, P]]

    def rec_top[E <: Tuple, P <: Session]()(using c: chan.CS[P *: E, Var[0]]^): (`Pair`[Unit, chan.CS[P *: E, P]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[(P, E), P]]

    def rec_pop[E <: Tuple, P <: Session, N <: Int]()(using c: chan.CS[P *: E, Var[S[N]]]^): (`Pair`[Unit, chan.CS[E, Var[N]]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[E, Var[N]]]

    def send[T, E <: Tuple, P <: Session](x: T)(using c: chan.CS[E, Send[T, P]]^): (`Pair`[Unit, chan.CS[E, P]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[E, P]]

    def recv[T, E <: Tuple, P <: Session]()(using c: chan.CS[E, Recv[T, P]]^): `Pair`[T, chan.CS[E, P]^] @kill(c) =
      ??? // (c.asInstanceOf[chan.CS[E, P]], 0.asInstanceOf[T])

    def left[E <: Tuple, L <: Session, R <: Session]()(using c: chan.CS[E, Select[L, R]]^): (`Pair`[Unit, chan.CS[E, L]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[E, L]]

    def right[E <: Tuple, L <: Session, R <: Session]()(using c: chan.CS[E, Select[L, R]]^): (`Pair`[Unit, chan.CS[E, R]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[E, R]]

    def branch[E <: Tuple, L <: Session, R <: Session, T](using c: chan.CS[E, Branch[L, R]]^)
      (l: (cl: chan.CS[E, L]^) ?=> T @kill(cl))(r: (cr: chan.CS[E, R]^) ?=> T @kill(cr)): (T) @kill(c) =
      if ??? then
        l(using c.asInstanceOf[chan.CS[E, L]])
      else
        r(using c.asInstanceOf[chan.CS[E, R]])

    // def branch2[E <: Tuple, L <: Session, R <: Session](using c: chan.CS[E, Branch[L, R]]^)
    //   [T](l: chan.CS[E, L]^ ?=> T^)(r: chan.CS[E, R]^ ?=> T^): (T^) @kill(c) =
    //   if ??? then
    //     l(using c.asInstanceOf[chan.CS[E, L]])
    //   else
    //     r(using c.asInstanceOf[chan.CS[E, R]])

    def close[E <: Tuple]()(using c: chan.CS[E, End]^): Unit =
      chan._isOpen = false

    def loop[E <: Tuple, P <: Session](using c: chan.CS[E, P]^)(cond: => Boolean)
    (body: (k: chan.CS[E, P]^) ?=> Option[chan.CS[E, P]^] @kill(k)): Unit @kill(c) =
      if cond then
        body(using c) match
          case Some(c) =>
            chan.loop(using c)(cond)(body)
          case None =>

type EchoSInner = Recv[String, Branch[Var[0], End]]
type EchoServer = Rec[EchoSInner]
type EchoCInner = Dual[EchoSInner]
type EchoClient = Dual[EchoServer]
type Simple = Send[Int, Recv[String, End]]

object EchoServer:
  def apply(chan: Chan): chan.withProtocol[EmptyTuple, EchoServer, Unit] =
    chan.rec_push()

    def recur(chan: Chan): chan.withProtocol[EchoSInner *: EmptyTuple, EchoSInner, Unit] =
      val msg = chan.recv()

      println(msg)
      chan.branch {
        chan.rec_top()
        recur(chan)
      } {
        chan.close()
      }
    recur(chan)

object EchoClient:
  def readLine(): String = ???

  def apply(chan: Chan): chan.withProtocol[EmptyTuple, EchoClient, Unit] =
    chan.rec_push()

    def recur(chan: Chan): chan.withProtocol[EchoCInner *: EmptyTuple, EchoCInner, Unit] =
      val msg = readLine()
      chan.send(msg)
      val goAgain = readLine()
      if (goAgain == "???") then
        chan.left()
        chan.rec_top()
        recur(chan)
      else
        chan.right()
        chan.close()

    recur(chan)

object Main:
  def echotest() =
    val (serverChan, clientChan) = Chan[EchoServer]()
    EchoServer(serverChan)
    EchoClient(clientChan)

  // def badApply(chan: Chan, c: chan.CS[Unit, EchoServer]^) =
  //   var cInner = chan.rec_push(c)
  //   var isOpen = true

  //   while (isOpen) {
  //     val (c2, msg) = chan.recv(cInner)
  //     println(msg)

  //     chan.offer(c2) match
  //       case Left(c) =>
  //         cInner = chan.rec_top(c)
  //       case Right(c) =>
  //         chan.close(c)
  //         isOpen = false
  //   }
  // end badApply

// object EchoClient:
//   def readLine(): CSring = "something"

//   def apply(c: Chan[Unit, EchoClient]^) =
//     val c2 = c.rec_push()

//     @tailrec def recur(c: Chan[(EchoCInner, Unit), EchoCInner]^): Unit @kill(c) =
//       val input = readLine()
//       val c2 = c.send(input)
//       c.send(input)
//       if (input == "exit") then
//         c2.right().close()
//       else
//         recur(c2.left().rec_top())
//     end recur
//     recur(c2)
//   end apply