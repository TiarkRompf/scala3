// From https://arxiv.org/pdf/2009.08769
import language.experimental.captureChecking
import caps.*
import typestate.*

class Drone:
  type Idle
  type Hovering
  type Flying

object Drone:
  def apply(): Sigma { type A = Drone; type B = a.Idle^ } =
    val drone = new Drone:
      type Idle = Unit
      type Hovering = Unit
      type Flying = Unit
    new Sigma:
      type A = Drone
      type B = a.Idle^
      val a: drone.type = drone
      val b: drone.Idle^ = ()

  extension (drone: Drone)
    def takeOff(): drone.Idle ?=!>? drone.Hovering =
      Sigma((), ().asInstanceOf[drone.Hovering])

    def land(): drone.Hovering ?=!>? drone.Idle =
      Sigma((), ().asInstanceOf[drone.Idle])

    // Equivalent of moveTo, from state Hovering to state Flying
    def startAt(x: Double, y: Double): drone.Hovering ?=!>? drone.Flying =
      Sigma((), ().asInstanceOf[drone.Flying])

    def moveTo(x: Double, y: Double): drone.Flying^ ?=> Unit = ()

    def stop(): drone.Flying ?=!>? drone.Hovering =
      Sigma((), ().asInstanceOf[drone.Hovering])

    def hasArrived[T]()(using c: drone.Flying^)
      (left: (drone.Flying^) ?=!> T)(right: (drone.Hovering^) ?=!> T): T =
      if false then
        left(using c)
      else
        right(using c.asInstanceOf[drone.Hovering])

object Main:
  def main(): Unit =
    val drone = Drone()
    drone.takeOff()
    drone.startAt(10, 20)

    var i = 0
    var j = 0
    def recur(drone: Drone): (drone.Flying^) ?=!> Unit =
      drone.hasArrived[Unit]() {
        i += 1
        j += 1
        recur(drone)
      } {
        drone.land()
      }
    recur(drone)