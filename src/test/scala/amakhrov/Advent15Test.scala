package amakhrov

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Advent15Test extends AnyFlatSpec with should.Matchers {
  "solve" should "handle example case 1" in {
    Solver15.solve(List("0", "3", "6"), 2020) should be(436)
  }
  "solve" should "handle example case 2" in {
    Solver15.solve(List("1", "3", "2"), 2020) should be(1)
  }
  "solve" should "handle example case 3" in {
    Solver15.solve(List("2", "1", "3"), 2020) should be(10)
  }
}
