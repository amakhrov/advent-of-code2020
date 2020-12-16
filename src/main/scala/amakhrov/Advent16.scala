package amakhrov

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.collection.mutable.{Map => MMap}

object Solver16 {
  def readResource(path: String): String =
    Using.resource(Source.fromResource(path)) { _.getLines().mkString("\n") }

  case class Range(min: Int, max: Int) {
    def contains(value: Int): Boolean = value >= min && value <= max
  }
  case class Rule(field: String, ranges: List[Range]) {
    def satisfies(value: Int): Boolean = ranges.exists(_.contains(value))
  }
  case class Ticket(values: Vector[Int])

  object Rule {
    def fromString(str: String): Rule = {
      val rx = raw"(.+?): (\d+)-(\d+) or (\d+)-(\d+)".r
      str match {
        case rx(field, min1, max1, min2, max2) =>
          Rule(
            field,
            List(Range(min1.toInt, max1.toInt), Range(min2.toInt, max2.toInt))
          )
        case _ => throw new Exception(s"Failed to parse '$str'")
      }
    }
  }
  object Ticket {
    def fromString(str: String): Ticket =
      Ticket(str.split(',').map(_.toInt).toVector)
  }

  def parse(data: String): (List[Rule], Ticket, List[Ticket]) = {
    val groups = data.split("\n\n")
    val rules = groups(0).split("\n").map(Rule.fromString).toList
    val myTicket = Ticket.fromString(groups(1).split("\n")(1))
    val tickets = groups(2).split("\n").toList.tail.map(Ticket.fromString)
    (rules, myTicket, tickets)
  }

  def solve(data: String): Long = {
    val (rules, myTicket, tickets) = parse(data)

    def isValidTicket(ticket: Ticket): Boolean =
      ticket.values.forall(value => rules.exists(_.satisfies(value)))

    val validTickets = tickets.filter(isValidTicket)

    val candidates = (for (i <- rules.indices) yield {
      val values = validTickets.map(_.values(i))
      val matchingRules = rules.filter(r => values.forall(r.satisfies))
      (i, matchingRules)
    }).sortBy(_._2.length)

    val matched = MMap[Rule, Int]()
    for ((i, rules) <- candidates) {
      val rule = rules.find(matched.contains(_) == false).get
      matched.put(rule, i)
    }

    val departures = rules.filter(_.field.startsWith("departure"))

    departures.map(matched).map(myTicket.values(_)).map(_.toLong).product
  }
}

object Advent16 extends App {
  println(
    Solver16.solve(Solver16.readResource("./puzzle16-real.txt"))
  )
}
