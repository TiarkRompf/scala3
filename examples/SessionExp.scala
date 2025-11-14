// Based on https://munksgaard.me/papers/laumann-munksgaard-larsen.pdf
// Performs operations on channels directly
import language.experimental.captureChecking
import caps.*
import scala.annotation.tailrec
import scala.compiletime.ops.int.S
import typestate.*

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