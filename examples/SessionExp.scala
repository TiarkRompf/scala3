// Based on https://munksgaard.me/papers/laumann-munksgaard-larsen.pdf
// Performs operations on channels directly
import language.experimental.captureChecking
import caps.*
import scala.annotation.tailrec
import scala.compiletime.ops.int.S
import typestate.*

trait Local
class Send[B, T <: Local] extends Local
class Recv[B, T <: Local] extends Local
class Select[L <: Local, R <: Local] extends Local
class Branch[L <: Local, R <: Local] extends Local
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

class Chan[E <: Tuple, T <: Local]
type EChan[T <: Local] = Chan[EmptyTuple, T]
type Emp[T <: Local] = T *: EmptyTuple

object Chan:
  def apply[T <: Local](): (EChan[T]^, EChan[Dual[T]]^) =
    (new Chan[EmptyTuple, T], new Chan[EmptyTuple, Dual[T]])

  extension [E <: Tuple, T <: Local](chan: Chan[E, Rec[T]]^)
   def rec_push(): (Chan[T *: E, T]^) @kill(chan) =
      chan.asInstanceOf[Chan[T *: E, T]]

  extension [E <: Tuple, T <: Local](chan: Chan[T *: E, Var[0]]^)
    def rec_top(): (Chan[T *: E, T]^) @kill(chan) =
      chan.asInstanceOf[Chan[T *: E, T]]

  extension [E <: Tuple, T <: Local, N <: Int](chan: Chan[T *: E, Var[S[N]]]^)
    def rec_pop(): (Chan[E, Var[N]]^) @kill(chan) =
      chan.asInstanceOf[Chan[E, Var[N]]]

  extension [E <: Tuple, T <: Local, B](chan: Chan[E, Send[B, T]]^)
    def send(x: B): (Chan[E, T]^) @kill(chan) =
      chan.asInstanceOf[Chan[E, T]]

  extension [E <: Tuple, T <: Local, B](chan: Chan[E, Recv[B, T]]^)
    def recv(): (Chan[E, T]^, B) @kill(chan) =
      ???

  extension [E <: Tuple, L <: Local, R <: Local](chan: (Chan[E, Select[L, R]]^))
    def left(): (Chan[E, L]^) @kill(chan) =
      chan.asInstanceOf[Chan[E, L]]

    def right(): (Chan[E, R]^) @kill(chan) =
      chan.asInstanceOf[Chan[E, R]]

  extension [E <: Tuple, L <: Local, R <: Local](chan: (Chan[E, Branch[L, R]]^))
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