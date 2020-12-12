package amakhrov

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Try, Using}

object Advent12 extends App {
  case class Command(action: Char, value: Int)

  object Command {
    def fromString(str: String): Command = {
      val rx = raw"([A-Z])(\d+)".r
      str match {
        case rx(action, value) => Command(action(0), value.toInt)
        case _                 => throw new Exception(s"failed to parse instruction $str")
      }
    }
  }

  case class Ship(x: Int, y: Int, wx: Int, wy: Int) {
    @tailrec
    final def handle(cmd: Command): Ship =
      cmd match {
        case Command('N', value) => Ship(x, y, wx, wy - value)
        case Command('S', value) => Ship(x, y, wx, wy + value)
        case Command('E', value) => Ship(x, y, wx + value, wy)
        case Command('W', value) => Ship(x, y, wx - value, wy)
        case Command('F', value) if value > 0 =>
          Ship(x + wx, y + wy, wx, wy).handle(Command('F', value - 1))
        case Command('F', _)     => this
        case Command('L', value) => rotate(-value)
        case Command('R', value) => rotate(value)
        case _ =>
          throw new Exception(
            s"Unsupported command '${cmd.action}${cmd.value}'"
          )
      }

    private def rotate(value: Int): Ship = {
      val (newWx, newWy) = rotateWaypoint(value)
      Ship(x, y, newWx, newWy)
    }

    private def rotateWaypoint(value: Int): (Int, Int) =
      (value + 360) % 360 match {
        case 0   => (wx, wy)
        case 90  => (-wy, wx)
        case 180 => (-wx, -wy)
        case 270 => (wy, -wx)
        case _   => throw new Exception(s"Unsupported rotation angle $value")
      }

    def distance: Int = x.abs + y.abs
  }

  object Ship {
    private val dirs = Map(0 -> 'E', 90 -> 'N', 180 -> 'W', 270 -> 'S')

    def initial(): Ship = Ship(0, 0, 10, -1)
  }

  val data =
    Using.resource(
      Source.fromFile("/Users/amakhrov/Downloads/puzzle12-real.txt")
    ) {
      _.getLines().toList
    }

  val instructions = data.map(Command.fromString)

  @tailrec
  def navigate(ship: Ship, cmds: Seq[Command]): Ship =
    cmds match {
      case cmd :: tail => navigate(ship.handle(cmd), tail)
      case Nil         => ship
    }

  val finalShip = navigate(Ship.initial(), instructions)
  println(finalShip)
  println(finalShip.distance)
}
