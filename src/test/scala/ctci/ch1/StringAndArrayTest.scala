package ctci.ch1

import org.scalatest.{FlatSpec, Matchers}
import StringAndArray._

class StringAndArrayTest extends FlatSpec with Matchers {

  "isAllUniqueFunctional" should "return true if all characters are unique" in {
    isAllUniqueFunctional("prat") shouldBe true
  }

  it should "return false if all characters are not unique" in {
    isAllUniqueFunctional("prateek") shouldBe false
  }

  "isAllUnique" should "return true if all characters are unique" in {
    isAllUnique("prat") shouldBe true
  }

  it should "return false if all characters are not unique" in {
    isAllUnique("prateek") shouldBe false
  }

  "isAllUniqueNoExtraSpace" should "return true if all characters are unique" in {
    isAllUniqueNoExtraSpace("prat") shouldBe true
  }

  it should "return false if all characters are not unique" in {
    isAllUniqueNoExtraSpace("prateek") shouldBe false
  }

  "isAllUniqueWithSort" should "return true if all characters are unique" in {
    isAllUniqueWithSort("prat") shouldBe true
  }

  it should "return false if all characters are not unique" in {
    isAllUniqueWithSort("prateek") shouldBe false
  }

  "isPermutation" should "return true if s2 is permutation of s1" in {
    isPermutation("prateek", "keepart") shouldBe true
  }

  it should "return false if s2 is not a permutation of s1" in {
    isPermutation("prateek", "keeport") shouldBe false
  }

  "urlify" should "return the urlified string" in {
    urlify("Mr John Smith    ", 13) shouldBe "Mr%20John%20Smith"
  }

  "urlifyNoExtraSpace" should "return the urlified string" in {
    urlifyNoExtraSpace("Mr John Smith    ", 13) shouldBe "Mr%20John%20Smith"
  }

  "palindromePerm" should "return true if permutations are palindrome" in {
    palindromePerm("tact coa") shouldBe true
  }

  it should "return true if permutations are palindrome are length is even" in {
    palindromePerm("tact coao") shouldBe true
  }

  it should "return false if permutations are not palindrome" in {
    palindromePerm("tact coaz") shouldBe false
  }

  "oneAway" should "return true for single modification" in {
    oneAway("pale", "ple") shouldBe true
    oneAway("pales", "pale") shouldBe true
    oneAway("pale", "bale") shouldBe true
  }

  "compress" should "return the compressed string" in {
    compress("aabcccccaaa") shouldBe "a2b1c5a3"

    compress("aabcccccaaax") shouldBe "a2b1c5a3x1"
  }

  "zeroMatrix" should "nullify the rows and collumns with 0" in {
    val m = Array(Array(1, 0, 1), Array(0, 1, 1), Array(1, 1, 1))
    zeroMatrix(m) shouldBe Array(Array(0, 0, 0), Array(0, 0, 0), Array(0, 0, 1))
  }
}
