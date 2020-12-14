package amakhrov

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Advent14Test extends AnyFlatSpec with should.Matchers {
  "solve" should "handle example case" in {
    Solver14.solve(Solver14.readResource("./puzzle14-test.txt")) should be(165)
  }

}
