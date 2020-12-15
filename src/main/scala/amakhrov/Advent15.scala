package amakhrov

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.collection.mutable.{Map => MMap}

object Solver15 {
  def readResource(path: String): List[String] =
    Using.resource(Source.fromResource(path)) {
      _.getLines().toList.head.split(',').toList
    }

  def solve(data: List[String], n: Int): Int = {
    val ages = {
      val map = data.dropRight(1).map(_.toInt).zipWithIndex.toMap
      MMap[Int, Int](map.toSeq: _*)
    }

    @tailrec
    def inner(prev: Int, i: Int): Int = {
      if (i == n)
        prev
      else {
        val prevTurn = i - 1
        val prevAge = ages.get(prev)
        ages.put(prev, prevTurn)
        prevAge match {
          case Some(age) => inner(prevTurn - age, i + 1)
          case None      => inner(0, i + 1)
        }
      }
    }

    inner(data.last.toInt, data.length)
  }
}

object Advent15 extends App {
  println(
    Solver15.solve(Solver15.readResource("./puzzle15-real.txt"), 30000000)
  )
}
