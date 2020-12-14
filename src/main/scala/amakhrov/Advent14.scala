package amakhrov

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.collection.mutable.{Map => MMap}

object Solver14 {
  def readResource(path: String): List[String] =
    Using.resource(Source.fromResource(path)) {
      _.getLines().toList
    }

  sealed trait Cmd
  case class Mask(mask: String) extends Cmd {
    def applyMask(value: Long): Long = {
      var res = value
      for ((bit, i) <- mask.reverse.toVector.zipWithIndex) {
        if (bit == '1') {
          res = res | (1L << i)
        } else if (bit == '0') {
          res = res & ~(1L << i)
        }
      }
      res
    }
  }
  case class Mem(address: Long, value: Long) extends Cmd {}
  object Cmd {
    def fromString(str: String): Cmd = {
      val memRx = raw"mem\[(\d+)\]".r
      str.split(" = ") match {
        case Array("mask", mask)          => Mask(mask)
        case Array(memRx(strAddr), value) => Mem(strAddr.toLong, value.toLong)
        case _                            => throw new Exception(s"Failed to parse '$str'")
      }
    }
  }

  def solve(data: List[String]): Long = {
    val mem = MMap[Long, Long]()
    var mask: Mask = null
    for (cmd <- data.map(Cmd.fromString)) cmd match {
      case m: Mask             => mask = m
      case Mem(address, value) => mem.addOne(address -> mask.applyMask(value))
    }

    println(mem)

    mem.values.sum
  }
}

object Advent14 extends App {
  println(Solver14.solve(Solver14.readResource("./puzzle14-real.txt")))
}
