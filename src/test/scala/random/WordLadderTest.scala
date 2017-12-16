package random

import org.scalatest.{FlatSpec, Matchers}
import WordLadder._

class WordLadderTest extends FlatSpec with Matchers {

  "countWordLadder" should "return the correct length" in {
    //countWordLadder("hit", "cog", Set("hot","dot","dog","lot","log","cog")) shouldBe 5
    countWordLadder("hit", "hot", Set("hot","dot","dog","lot","log","cog")) shouldBe 2
  }

}
