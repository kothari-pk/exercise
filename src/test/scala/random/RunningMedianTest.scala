package random

import org.scalatest.{FlatSpec, Matchers}
import RunningMedian._

class RunningMedianTest extends FlatSpec with Matchers {

  "getMedians" should "return the correct median" in {
    getMedians(Array(1, 2, 3, 4, 5)) shouldBe Array(1, 1.5, 2, 2.5, 3)
  }

  "getMedianStream" should "return the correct median" in {
    getMedianStream(Stream(1, 2, 3, 4, 5)).toList shouldBe List(1, 1.5, 2, 2.5, 3)
  }

}
