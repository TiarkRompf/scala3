// Based on https://munksgaard.me/papers/laumann-munksgaard-larsen.pdf
// Iteration 2 - each channel has an associated path-dependent capability parameterized by the channel CSate.
import language.experimental.captureChecking
import caps.*
import typestate.*
import scala.annotation.tailrec
import scala.compiletime.ops.int.*

// def loopS[E, P <: Session](chan: Chan, c: chan.CS[E, P]^)(cond: => Boolean)
//   (body: (c: chan.CS[E, P]^) => Option[chan.CS[E, P]^] @kill(c)): Unit @kill(c) =
//     if cond then
//       body(c) match
//         case Some(c) =>
//           loopS(chan, c)(cond)(body)
//         case None =>

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

  private var _isOpen = true
  def isOpen = _isOpen

object Chan:
  // def apply[P <: Session]():

  extension (chan: Chan)
    def rec_push[E <: Tuple, P <: Session](using c: chan.CS[E, Rec[P]]^): (`Pair`[Unit, chan.CS[P *: E, P]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[P *: E, P]]

    def rec_top[E <: Tuple, P <: Session](using c: chan.CS[P *: E, Var[0]]): (`Pair`[Unit, chan.CS[P *: E, P]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[(P, E), P]]

    def rec_pop[E <: Tuple, P <: Session, N <: Int](using c: chan.CS[P *: E, Var[S[N]]]): (`Pair`[Unit, chan.CS[E, Var[N]]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[E, Var[N]]]

    def send[T, E <: Tuple, P <: Session](x: T)(using c: chan.CS[E, Send[T, P]]^): (`Pair`[Unit, chan.CS[E, P]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[E, P]]

    def recv[T, E <: Tuple, P <: Session]()(using c: chan.CS[E, Recv[T, P]]^): `Pair`[T, chan.CS[E, P]^] @kill(c) =
      ??? // (c.asInstanceOf[chan.CS[E, P]], 0.asInstanceOf[T])

    def left[E <: Tuple, L <: Session, R <: Session](using c: chan.CS[E, Select[L, R]]^): (`Pair`[Unit, chan.CS[E, L]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[E, L]]

    def right[E <: Tuple, L <: Session, R <: Session](using c: chan.CS[E, Select[L, R]]^): (`Pair`[Unit, chan.CS[E, R]^]) @kill(c) =
      ??? // c.asInstanceOf[chan.CS[E, R]]

    def branch[E <: Tuple, L <: Session, R <: Session, T, U^](using c: chan.CS[E, Branch[L, R]]^)
      (l: chan.CS[E, L]^ ?=> T^{U})(r: chan.CS[E, R]^ ?=> T^{U}): (T^{U}) @kill(c) =
      if ??? then
        l(using c.asInstanceOf[chan.CS[E, L]])
      else
        r(using c.asInstanceOf[chan.CS[E, R]])

    def branch2[E <: Tuple, L <: Session, R <: Session](using c: chan.CS[E, Branch[L, R]]^):
      (Either[chan.CS[E, L]^, chan.CS[E, R]^]) @kill(c) =
      ???

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

object Test:
  def a(chan: Chan): chan.withProtocol[EmptyTuple, Simple, Unit] =
    chan.send(10)
    val x = chan.recv()
    chan.close()
    x

//   def apply(chan: Chan, c: chan.CS[Unit, EchoServer]^) =
//   //   loop1[Option[chan.CS[(EchoSInner, Unit), EchoSInner]^]](Some(cInner))(chan.isOpen) { oc =>
//   //     oc.flatMap { c =>
//   //       val (c2, msg) = chan.recv(cInner)
//   //       println(msg)

//   //       chan.offer(c2) match
//   //         case Left(c) =>
//   //           Some(chan.rec_top(c))
//   //         case Right(c) =>
//   //           chan.close(c)
//   //           None
//   //     }
//   //  }
//   end apply
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

// object Main:
//   import CCHack.*
//   def echo_teCS() =
//     val channels = Chan[EchoServer]()
//     val server_chan = assumeFresh(channels._1)
//     val client_chan = assumeFresh(channels._2)
//     EchoServer(server_chan)
//     EchoClient(client_chan)