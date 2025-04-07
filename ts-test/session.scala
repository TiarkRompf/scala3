// Based on https://munksgaard.me/papers/laumann-munksgaard-larsen.pdf
package typestate

import language.experimental.captureChecking
import scala.annotation, annotation.tailrec

class kill(xs: Any*) extends annotation.StaticAnnotation

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

class Chan[E, S <: Protocol] // maybe also constrain E?
// instead of having type parameter protocol, make it have abstract type member with protocol

object Chan:
  def apply[P <: Protocol](): (Chan[Unit, P], Chan[Unit, Dual[P]]) =
    (new Chan[Unit, P],
     new Chan[Unit, Dual[P]])

  extension [E, P <: Protocol](chan: Chan[E, Rec[P]]) // basically we want to kill for all these extension methods
   def enter(): Chan[(P, E), P] =
      chan.asInstanceOf[Chan[(P, E), P]]

  extension [E, P <: Protocol](chan: Chan[(P, E), Var[Z]])
    def zero(): Chan[(P, E), P] =
      chan.asInstanceOf[Chan[(P, E), P]]

  extension [E, P <: Protocol, N <: Nat](chan: Chan[(P, E), Var[S[N]]])
    def succ(): Chan[E, Var[N]] =
      chan.asInstanceOf[Chan[E, Var[N]]]

  extension [E, T, P <: Protocol](chan: Chan[E, Send[T, P]])
    def send(x: T): Chan[E, P] =
      chan.asInstanceOf[Chan[E, P]]

  extension [E, T, P <: Protocol](chan: Chan[E, Recv[T, P]])
    def recv(): (Chan[E, P], T) =
      chan.asInstanceOf[(Chan[E, P], T)]

  extension [E, L <: Protocol, R <: Protocol](chan: Chan[E, Choose[L, R]])
    def left(): Chan[E, L] =
      chan.asInstanceOf[Chan[E, L]]

  extension [E, L <: Protocol, R <: Protocol](chan: Chan[E, Choose[L, R]])
    def right(): Chan[E, R] =
      chan.asInstanceOf[Chan[E, R]]

  extension [E, L <: Protocol, R <: Protocol](chan: Chan[E, Offer[L, R]])
    def offer(): Either[Chan[E, L], Chan[E, R]] =
      Left(chan.asInstanceOf[Chan[E, L]])

  extension [E](chan: Chan[E, Close])
    def close(): Unit = ()

type AtmDeposit = Recv[Int, Send[Int, Var[Z]]]
type AtmWithdraw = Recv[Int, Choose[Var[Z], Var[Z]]]
type AtmInner = Offer[AtmDeposit, Offer[AtmWithdraw, Close]]
type Atm = Recv[String, Choose[Rec[AtmInner], Close]]

object Atm:
  private def approved(id: String) = true
  private def updateBal(amt: Int): Int = amt + 10

  def apply(c: Chan[Unit, Atm]): Unit =
    val (c1, id) = c.recv()
    if !approved(id) then
      c1.right().close()
      return
    else
      var c = c1.left().enter()
      var isOpen = true
      while (isOpen) do // not nice
        c.offer() match
          case Left(c1) =>
            val (c2, amt) = c1.recv()
            c = c2.send(updateBal(amt)).zero()
          case Right(c1) =>
            c1.offer() match
              case Left(c1) =>
                val (c2, amt) = c1.recv()
                if (amt <= 10) then
                  c = c2.left().zero()
                else
                  c = c2.right().zero()
              case Right(c1) =>
                c1.close()
                isOpen = false
      // @tailrec def recur[E, P <: Protocol](c: Chan[(P, E), Offer[AtmDeposit, Offer[AtmWithdraw, Close]]]): Unit =
      //   c.offer() match
      //     case Left(c) =>
      //       val (c2, amt) = c.recv()
      //       recur(c2.send(updateBal(amt)).zero())
      //     case Right(c) =>
      //       c.offer() match
      //         case Left(c) =>
      //           val (c2, amt) = c.recv()
      //           if 10 >= amt then
      //             recur(c2.left().zero())
      //           else
      //             recur(c2.right().zero())
      //         case Right(c) =>
      //           c.close()
      // end recur
      // recur(c2)

type Client = Dual[Atm]
type ClientInner = Dual[AtmInner]

object Client:
  val id: String = "A"
  def clientDeposit(c: Chan[Unit, Client]): Unit =
    val c2 = c.send(id).offer() match
      case Left(c) => c.enter()
      case Right(c) =>
        c.close()
        return

    val (c3, new_bal) = c2.left().send(100).recv()
    println(s"New Balance: ${new_bal}")
    c3.zero().right().right().close()

object Main:
  import Client.clientDeposit
  def main() =
    val (atm_chan, client_chan) = Chan[Atm]() // create new channels
    Atm(atm_chan) // start atm server
    clientDeposit(client_chan) // start a client
