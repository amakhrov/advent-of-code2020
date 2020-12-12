package amakhrov

import amakhrov.Advent11.Field.{EMPTY, FLOOR, OCCUPIED}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Try, Using}

object Advent11 extends App {
  type Cell = Char

  case class Field(cells: Vector[Vector[Cell]]) {
    def rows: Int = cells.length
    def cols: Int = cells(0).length
    def occupied: Int = cells.flatten.count(_ == OCCUPIED)

    private def cellOption(row: Int, col: Int): Option[Cell] =
      Try { cells(row)(col) }.toOption

    private def dirs = {
      val ds = List(-1, 0, 1)
      for (r <- ds; c <- ds if !(r == 0 && c == 0))
        yield (r, c)
    }

    def getNeighbors(row: Int, col: Int): List[Cell] =
      for (
        (dr, dc) <- dirs;
        cell <- cellOption(row + dr, col + dc)
      ) yield cell

    def getVisibleNeighbors(row: Int, col: Int): List[Cell] = {
      @tailrec
      def findSeat(dr: Int, dc: Int, r: Int = row, c: Int = col): Option[Cell] =
        cellOption(r + dr, c + dc) match {
          case Some(FLOOR) => findSeat(dr, dc, r + dr, c + dc)
          case seat        => seat
        }

      for ((dr, dc) <- dirs; seat <- findSeat(dr, dc)) yield seat
    }

    def countOccupiedNeighbors(row: Int, col: Int): Int =
      getNeighbors(row, col).count(_ == OCCUPIED)

    def calcNext(): Field =
      Field(Vector.tabulate(rows, cols) { (row, col) =>
        (cells(row)(col), countOccupiedNeighbors(row, col)) match {
          case (EMPTY, 0)              => OCCUPIED
          case (OCCUPIED, n) if n >= 4 => EMPTY
          case (cell, _)               => cell
        }
      })

    def print(): Unit = cells.foreach(row => println(row.mkString))
  }

  object Field {
    val FLOOR = '.'
    val EMPTY = 'L'
    val OCCUPIED = '#'

    def fromString(str: String): Field =
      Field(str.split('\n').toVector.map(_.toVector))
  }

  val data =
    Using.resource(
      Source.fromFile("/Users/amakhrov/Downloads/puzzle11-real.txt")
    ) {
      _.getLines().mkString("\n")
    }

  val field = Field.fromString(data)

  @tailrec
  def findStable(field: Field, maxTries: Int = 1000): Field =
    field.calcNext() match {
      case next if next == field => field
      case next if maxTries > 0  => findStable(next, maxTries - 1)
      case _                     => throw new Exception("reached max tries")
    }

  println(findStable(field).occupied)
}
