// Based on https://munksgaard.me/papers/laumann-munksgaard-larsen.pdf
// Iteration 2 - each channel has an associated path-dependent capability parameterized by the channel state.
package typestate

import language.experimental.captureChecking
import caps.*, unsafe.*
import scala.annotation, annotation.tailrec

class kill(xs: Any*) extends annotation.StaticAnnotation

// object CCHack:
//   def assumeFresh[E, P <: Session](x: Chan[E, P]): Chan[E, P]^ =
//     x.asInstanceOf[Chan[E, P]^]

// problem - cannot instantiate T with a fresh capability.
def loop1[T](x: T)(cond: => Boolean)(body: (y: T) ->{cap} T @kill(y)): Unit @kill(x) =
  if cond then
    val y = body(x)
    loop1(y)(cond)(body)

// problem is we cannot instantiate U with the channel capability
// def loop2[T, U](x: T)(cond: => Boolean)(body: (x: T) => Either[T, U] @kill(x)): U @kill(x) =
//   if cond then
//     body(x) match
//       case Left(x) => loop2(x)(cond)(body)
//       case Right(y) => y

def loopS[E, P <: Session](chan: Chan, c: chan.Protocol[E, P]^)(cond: => Boolean)
  (body: (c: chan.Protocol[E, P]^) => Option[chan.Protocol[E, P]^] @kill(c)): Unit @kill(c) =
    if cond then
      body(c) match
        case Some(c) =>
          loopS(chan, c)(cond)(body)
        case None =>


trait Nat
class Z extends Nat
class S[N <: Nat] extends Nat

trait Session
class Send[T, P <: Session] extends Session
class Recv[T, P <: Session] extends Session
class Choose[L <: Session, R <: Session] extends Session
class Offer[L <: Session, R <: Session] extends Session
class Rec[P <: Session] extends Session
class Var[N <: Nat] extends Session
class Close extends Session

type Dual[P <: Session] <: Session = P match
  case Send[t, p] => Recv[t, Dual[p]]
  case Recv[t, p] => Send[t, Dual[p]]
  case Choose[l, r] => Offer[Dual[l], Dual[r]]
  case Offer[l, r] => Choose[Dual[l], Dual[r]]
  case Rec[p] => Rec[Dual[p]]
  case Var[n] => Var[n]
  case Close => Close

class Chan:
  type Protocol[E, P <: Session]
  private var _isOpen = true
  def isOpen = _isOpen

object Chan:
  extension (chan: Chan)
    def make[P <: Session](): (chan.Protocol[Unit, P]^, chan.Protocol[Unit, Dual[P]]^) =
      (0.asInstanceOf[chan.Protocol[Unit, P]], 1.asInstanceOf[chan.Protocol[Unit, Dual[P]]])

    def rec_push[E, P <: Session](c: chan.Protocol[E, Rec[P]]^): (chan.Protocol[(P, E), P]^) @kill(c) =
      c.asInstanceOf[chan.Protocol[(P, E), P]]

    def rec_top[E, P <: Session](c: chan.Protocol[(P, E), Var[Z]]): (chan.Protocol[(P, E), P]) @kill(c) =
      c.asInstanceOf[chan.Protocol[(P, E), P]]

    def rec_pop[E, P <: Session, N <: Nat](c: chan.Protocol[(P, E), Var[S[N]]]): (chan.Protocol[E, Var[N]]) @kill(c) =
      c.asInstanceOf[chan.Protocol[E, Var[N]]]

    def send[E, P <: Session, T](x: T, c: chan.Protocol[E, Send[T, P]]^): (chan.Protocol[E, P]^) @kill(c) =
      c.asInstanceOf[chan.Protocol[E, P]]

    def recv[E, P <: Session, T](c: chan.Protocol[E, Recv[T, P]]^): (chan.Protocol[E, P], T^) @kill(c) =
      (c.asInstanceOf[chan.Protocol[E, P]], 0.asInstanceOf[T])

    def left[E, L <: Session, R <: Session](c: chan.Protocol[E, Choose[L, R]]^): (chan.Protocol[E, L]^) @kill(c)=
      c.asInstanceOf[chan.Protocol[E, L]]

    def right[E, L <: Session, R <: Session](c: chan.Protocol[E, Choose[L, R]]^): (chan.Protocol[E, R]^) @kill(c) =
      c.asInstanceOf[chan.Protocol[E, R]]

    // this one is a problem
    // how to return Either implicitly?
    def offer[E, L <: Session, R <: Session](c: chan.Protocol[E, Offer[L, R]]^):
        (Either[chan.Protocol[E, L]^, chan.Protocol[E, R]^]) @kill(c) =
      Left(c.asInstanceOf[chan.Protocol[E, L]])

    def close[E](c: chan.Protocol[E, Close]^): Unit =
      chan._isOpen = false

type EchoSInner = Recv[String, Offer[Var[Z], Close]]
type EchoServer = Rec[EchoSInner]
type EchoCInner = Dual[EchoSInner]
type EchoClient = Dual[EchoServer]

object EchoServer:
  def apply(chan: Chan, c: chan.Protocol[Unit, EchoServer]^) =
    val cInner = chan.rec_push(c)

  //   loop1[Option[chan.Protocol[(EchoSInner, Unit), EchoSInner]^]](Some(cInner))(chan.isOpen) { oc =>
  //     oc.flatMap { c =>
  //       val (c2, msg) = chan.recv(cInner)
  //       println(msg)

  //       chan.offer(c2) match
  //         case Left(c) =>
  //           Some(chan.rec_top(c))
  //         case Right(c) =>
  //           chan.close(c)
  //           None
  //     }
  //  }
  end apply
  // def badApply(chan: Chan, c: chan.Protocol[Unit, EchoServer]^) =
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
//   def readLine(): String = "something"

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
//   def echo_test() =
//     val channels = Chan[EchoServer]()
//     val server_chan = assumeFresh(channels._1)
//     val client_chan = assumeFresh(channels._2)
//     EchoServer(server_chan)
//     EchoClient(client_chan)