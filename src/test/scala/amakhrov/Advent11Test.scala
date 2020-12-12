package amakhrov
import amakhrov.Advent11.Field
import org.scalatest._
import org.scalatest.flatspec._
import matchers._

class Advent11Test extends AnyFlatSpec with should.Matchers {
  "Field.getNeightbours" should "handle edges" in {
    val data = """#.##.##.##
                 |#######.##
                 |#.#.#..#..
                 |####.##.##
                 |#.##.##.##
                 |#.#####.##
                 |..#.#.....
                 |##########
                 |#.######.#
                 |#.#####.##""".stripMargin
    val field = Field.fromString(data)
    field.rows should be(10)
    field.cols should be(10)
    field.getNeighbors(0, 9) should be(List('#', '#', '#'))
  }

}
