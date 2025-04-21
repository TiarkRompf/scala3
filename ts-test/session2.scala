// Based on https://munksgaard.me/papers/laumann-munksgaard-larsen.pdf
// Iteration 2 - each channel has an associated path-dependent capability parameterized by the channel state.
package typestate

import language.experimental.captureChecking
import caps.*, unsafe.*
import scala.annotation, annotation.tailrec

class kill(xs: Any*) extends annotation.StaticAnnotation

// object CCHack:
//   def assumeFresh[E, P <: Protocol](x: Chan[E, P]): Chan[E, P]^ =
//     x.asInstanceOf[Chan[E, P]^]

trait Nat
class Z extends Nat
class S[A <: Nat] extends Nat

trait Protocol
class Send[T, P <: Protocol] extends Protocol
class Recv[T, P <: Protocol] extends Protocol
class Choose[L <: Protocol, R <: Protocol] extends Protocol
class Offer[L <: Protocol, R <: Protocol] extends Protocol
class Rec[P <: Protocol] extends Protocol
class Var[N <: Nat] extends Protocol
class Close extends Protocol

type Dual[P <: Protocol] <: Protocol = P match
  case Send[t, p] => Recv[t, Dual[p]]
  case Recv[t, p] => Send[t, Dual[p]]
  case Choose[l, r] => Offer[Dual[l], Dual[r]]
  case Offer[l, r] => Choose[Dual[l], Dual[r]]
  case Rec[p] => Rec[Dual[p]]
  case Var[n] => Var[n]
  case Close => Close

class Chan:
  type Proto[E, P <: Protocol]


object Chan:
  extension (chan: Chan)
    def make[P <: Protocol](): (chan.Proto[Unit, P], chan.Proto[Unit, Dual[P]]) =
      (0.asInstanceOf[chan.Proto[Unit, P]], 1.asInstanceOf[chan.Proto[Unit, Dual[P]]])

    def rec_push[E, P <: Protocol](c: chan.Proto[E, Rec[P]]): (chan.Proto[(P, E), P]) =
      c.asInstanceOf[chan.Proto[(P, E), P]]

    def rec_top[E, P <: Protocol](c: chan.Proto[(P, E), Var[Z]]): (chan.Proto[(P, E), P]) =
      c.asInstanceOf[chan.Proto[(P, E), P]]

    def rec_pop[E, P <: Protocol, N <: Nat](c: chan.Proto[(P, E), Var[S[N]]]): (chan.Proto[E, Var[N]]) =
      c.asInstanceOf[chan.Proto[E, Var[N]]]

    def send[E, P <: Protocol, T](c: chan.Proto[E, Send[T, P]]): (chan.Proto[E, P]) =
      c.asInstanceOf[chan.Proto[E, P]]

    def recv[E, P <: Protocol, T](c: chan.Proto[E, Recv[T, P]]): (chan.Proto[E, P], T) =
      (c.asInstanceOf[chan.Proto[E, P]], 0.asInstanceOf[T])

    def left[E, L <: Protocol, R <: Protocol](c: chan.Proto[E, Choose[L, R]]): (chan.Proto[E, L]) =
      c.asInstanceOf[chan.Proto[E, L]]

    def right[E, L <: Protocol, R <: Protocol](c: chan.Proto[E, Choose[L, R]]): (chan.Proto[E, R]) =
      c.asInstanceOf[chan.Proto[E, R]]

    // this one is a problem
    // how to return Either implicitly?
    def offer[E, L <: Protocol, R <: Protocol](c: chan.Proto[E, Offer[L, R]]): Either[chan.Proto[E, L], chan.Proto[E, R]] =
      Left(c.asInstanceOf[chan.Proto[E, L]])

    def close[E](c: chan.Proto[E, Close]): Unit = ()

type EchoSInner = Recv[String, Offer[Var[Z], Close]]
type EchoServer = Rec[EchoSInner]
type EchoCInner = Dual[EchoSInner]
type EchoClient = Dual[EchoServer]

object EchoServer:
  def apply(chan: Chan, c: chan.Proto[Unit, EchoServer]) =
    var cInner = chan.rec_push(c)
    var isOpen = true

    while (isOpen) {
      val (c2, msg) = chan.recv(cInner)
      println(msg)

      chan.offer(c2) match
        case Left(c) =>
          cInner = chan.rec_top(c)
        case Right(c) =>
          chan.close(c)
          isOpen = false
    }
  end apply

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