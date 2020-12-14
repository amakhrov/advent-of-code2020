package amakhrov

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import amakhrov.Solver14p2.Mask

class Advent14p2Test extends AnyFlatSpec with should.Matchers {
  "Mask.applyMask" should "return all possible results using floating bits" in {
    def bin(s: String): Long = BigInt(s, 2).toLong

    Mask("000000000000000000000000000000X1001X")(
      bin("000000000000000000000000000000101010")
    ) should be(
      List(
        bin("000000000000000000000000000000011010"),
        bin("000000000000000000000000000000011011"),
        bin("000000000000000000000000000000111010"),
        bin("000000000000000000000000000000111011")
      )
    )
  }

}
