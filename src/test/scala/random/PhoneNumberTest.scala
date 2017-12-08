package random

import org.scalatest.{FlatSpec, Matchers}
import PhoneNumber._

class PhoneNumberTest extends FlatSpec with Matchers {

  "allWords" should "return all possible combinations 2 nums" in {
    allWords("23") shouldBe List("ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf")
  }
}
