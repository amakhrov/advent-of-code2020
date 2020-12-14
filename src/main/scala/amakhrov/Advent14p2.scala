package amakhrov

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.collection.mutable.{Map => MMap}

object Solver14p2 {
  def readResource(path: String): List[String] =
    Using.resource(Source.fromResource(path)) { _.getLines().toList }

  sealed trait Cmd
  case class Mask(mask: String) extends Cmd {
    def apply(value: Long): List[Long] = {
      val bits = mask.reverse.toVector.zipWithIndex
        .map(pair => {
          val (bit, i) = pair
          if (bit == '0')
            value >> i & 1
          else if (bit == '1')
            1
          else
            2
        })
        .reverse
        .toList

      for (bs <- permutate(bits)) yield BigInt(bs.mkString(""), 2).toLong
    }

    private def permutate(
        bits: List[Long]
    ): List[List[Long]] =
      bits match {
        case head :: tail =>
          for (
            l <- permutate(tail);
            b <- if (head == 2) List[Long](0, 1) else List(head)
          ) yield b :: l
        case Nil => List(Nil)
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
      case m: Mask => mask = m
      case Mem(address, value) =>
        for (addr <- mask(address)) mem.addOne(addr -> value)
    }

    mem.values.sum
  }
}

object Advent14p2 extends App {
  println(Solver14p2.solve(Solver14p2.readResource("./puzzle14-real.txt")))
}
