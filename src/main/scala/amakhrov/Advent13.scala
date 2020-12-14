package amakhrov

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Advent13 extends App {

  val data = Using.resource(Source.fromResource("./puzzle13-real.txt")) {
    _.getLines().toVector
  }

  val pairs = data(1)
    .split(",")
    .zipWithIndex
    .filter(_._1 != "x")
    .map(d => (d._1.toInt, d._2))
    .toList

  // terms
  // - offset - number of minutes from some reference point TS to the nearest departure of a bus

  // this is inefficient and won't complete in reasonable time
  def find(): Long = {
    val buses = pairs.map(_._1).sorted.reverse
    val earliestOffset = pairs.minBy(_._2)._2
    val offsets = pairs.toMap

    @tailrec
    def inner(ts: Long = buses.head - offsets(buses.head)): Long = {
      val matched = buses.tail.forall(b => (ts + offsets(b)) % b == 0)

      if (matched)
        ts + earliestOffset
      else if (ts > 867200349647749L) {
        throw new Exception("Not found")
      } else
        inner(ts + buses.head)
    }

    inner(buses.head - offsets(buses.head))
  }

  // fast implementation
  def find2(): Long = {
    @tailrec
    def findFirstMatchingTs(
        el: Int,
        desiredOffset: Int,
        maxTs: Long,
        period: Long,
        ts: Long = 0
    ): Long = {
      def getOffset(ts: Long): Long = (el - ts % el) % el

      if (ts >= maxTs)
        throw new Exception(s"Reached max ts when processing $el")
      else if (getOffset(ts) == desiredOffset % el)
        ts
      else
        findFirstMatchingTs(el, desiredOffset, maxTs, period, ts + period)
    }

    def inner(els: List[(Int, Int)]): (Long, Long) = {
      els match {
        case List(head) => (-head._2, head._1)
        case (bus, desiredOffset) :: tail =>
          val (firstTs, period) = inner(tail)
          println(s"Res for tail $tail: min=$firstTs, period=$period")
          val nextPeriod = period * bus

          (
            findFirstMatchingTs(
              bus,
              desiredOffset,
              nextPeriod,
              period,
              firstTs
            ),
            nextPeriod
          )
      }
    }

    inner(pairs)._1
  }

  println(find2())
}
