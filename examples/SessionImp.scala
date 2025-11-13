// Based on https://munksgaard.me/papers/laumann-munksgaard-larsen.pdf
// Uses channel itself directly
import language.experimental.captureChecking
import caps.*
import typestate.*
import scala.annotation.tailrec
import scala.compiletime.ops.int.S

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

class Chan[E <: Tuple, P <: Session]
type EChan[P <: Session] = Chan[EmptyTuple, P]
type Emp[P <: Session] = P *: EmptyTuple

object Chan:
  def apply[P <: Session](): (EChan[P]^, EChan[Dual[P]]^) =
    (new Chan[EmptyTuple, P], new Chan[EmptyTuple, Dual[P]])

  extension [E <: Tuple, P <: Session](chan: Chan[E, Rec[P]]^)
   def rec_push(): (Chan[P *: E, P]^) @kill(chan) =
      chan.asInstanceOf[Chan[P *: E, P]]

  extension [E <: Tuple, P <: Session](chan: Chan[P *: E, Var[0]]^)
    def rec_top(): (Chan[P *: E, P]^) @kill(chan) =
      chan.asInstanceOf[Chan[P *: E, P]]

  extension [E <: Tuple, P <: Session, N <: Int](chan: Chan[P *: E, Var[S[N]]]^)
    def rec_pop(): (Chan[E, Var[N]]^) @kill(chan) =
      chan.asInstanceOf[Chan[E, Var[N]]]

  extension [E <: Tuple, P <: Session, T](chan: Chan[E, Send[T, P]]^)
    def send(x: T): (Chan[E, P]^) @kill(chan) =
      chan.asInstanceOf[Chan[E, P]]

  extension [E <: Tuple, P <: Session, T](chan: Chan[E, Recv[T, P]]^)
    def recv(): (Chan[E, P]^, T) @kill(chan) =
      ???

  extension [E <: Tuple, L <: Session, R <: Session](chan: (Chan[E, Select[L, R]]^))
    def left(): (Chan[E, L]^) @kill(chan) =
      chan.asInstanceOf[Chan[E, L]]

    def right(): (Chan[E, R]^) @kill(chan) =
      chan.asInstanceOf[Chan[E, R]]

  // extension [E <: Tuple, L <: Session, R <: Session, T](chan: Chan[E, Branch[L, R]]^)
  //   def branch(left: (c: Chan[E, L]^) => T @kill(c))
  //             (right: (c: Chan[E, R]^) => T @kill(c)): T @kill(chan) =
  //     ???

  extension [E <: Tuple, L <: Session, R <: Session](chan: (Chan[E, Branch[L, R]]^))
    def branch(): Either[Chan[E, L]^, Chan[E, R]^] @kill(chan) =
      ???

  extension [E <: Tuple](chan: Chan[E, End]^)
    def close(): Unit @kill(chan) = ()

type EchoSInner = Recv[String, Branch[Var[0], End]]
type EchoServer = Rec[EchoSInner]
type EchoCInner = Dual[EchoSInner]
type EchoClient = Dual[EchoServer]

object EchoServer:
  def apply(c: EChan[EchoServer]^) =
    val c2 = c.rec_push()

    def recur(c: Chan[Emp[EchoSInner], EchoSInner]^): Unit @kill(c) = {
      val (c2, str) = c.recv()
      println(str)
      c2.branch() match
        case Left(c) =>
          recur(c.rec_top())
        case Right(c) =>
          c.close()
    }

    recur(c2)
  end apply

object EchoClient:
  def readLine(): String = ""

  def apply(c: EChan[EchoClient]^) =
    val c2 = c.rec_push()

    @tailrec def recur(c: Chan[Emp[EchoCInner], EchoCInner]^): Unit @kill(c) =
      val input = readLine()
      val c2 = c.send(input)

      if (input == "exit") then
        c2.right().close()
      else
        recur(c2.left().rec_top())
    end recur
    recur(c2)
  end apply

object Main:
  def echo_test() =
    val (serverChan, clientChan) = Chan[EchoServer]()
    EchoServer(serverChan)
    EchoClient(clientChan)

/*
type AtmDeposit = Recv[Int, Send[Int, Var[0]]]
type AtmWithdraw = Recv[Int, Select[Var[0], Var[0]]]
type AtmInner = Branch[AtmDeposit, Branch[AtmWithdraw, End]]
type Atm = Recv[String, Select[Rec[AtmInner], End]]

type Client = Dual[Atm]
type ClientInner = Dual[AtmInner]

object Atm:
  private def approved(id: String) = true
  private def updateBal(amt: Int): Int = amt + 10

  def apply(c: Chan[Unit, Atm]): Unit =
    val (c1, id) = c.recv()
    if !approved(id) then
      c1.right().close()
      return
    else
      val c2 = c1.left().rec_push()
      @tailrec def recur(c: Chan[(AtmInner, Unit), AtmInner]): Unit =
        c.offer() match
          case Left(c) =>
            val (c2, amt) = c.recv()
            recur(c2.send(updateBal(amt)).rec_top())
          case Right(c) =>
            c.offer() match
              case Left(c) =>
                val (c2, amt) = c.recv()
                if 10 >= amt then
                  recur(c2.left().rec_top())
                else
                  recur(c2.right().rec_top())
              case Right(c) =>
                c.close()
      end recur
      recur(c2)
      // var isOpen = true
      // while (isOpen) do // not nice
      //   c.offer() match
      //     case Left(c1) =>
      //       val (c2, amt) = c1.recv()
      //       c = c2.send(updateBal(amt)).rec_top()
      //     case Right(c1) =>
      //       c1.offer() match
      //         case Left(c1) =>
      //           val (c2, amt) = c1.recv()
      //           if (amt <= 10) then
      //             c = c2.left().rec_top()
      //           else
      //             c = c2.right().rec_top()
      //         case Right(c1) =>
      //           c1.close()
      //           isOpen = false

type AtmClient = Dual[Atm]
type AtmClientInner = Dual[AtmInner]

object AtmClient:
  val id: String = "A"
  def clientDeposit(c: Chan[Unit, AtmClient]): Unit =
    val c2 = c.send(id).offer() match
      case Left(c) => c.rec_push()
      case Right(c) =>
        c.close()
        return

    val (c3, new_bal) = c2.left().send(100).recv()
    println(s"New Balance: ${new_bal}")
    c3.rec_top().right().right().close()

object Main:
  // import AtmClient.clientDeposit
  // def atm_test() =
  //   val (atm_chan, client_chan) = Chan[Atm]() // create new channels
  //   Atm(atm_chan) // start atm server
  //   clientDeposit(client_chan) // start a client
*/
