package matrix

import org.scalatest.{FlatSpec, Matchers}
import SpiralOrder._

class SpiralOrderTest extends FlatSpec with Matchers {

  "spiralOrder" should "print the matrix is spiral order" in {
    val m: Array[Array[Int]] = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))

    spiralOrder(m) shouldBe List(1, 2, 3, 6, 9, 8, 7, 4, 5)
  }

}
