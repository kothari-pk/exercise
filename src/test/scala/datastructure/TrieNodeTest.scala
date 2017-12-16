package datastructure

import org.scalatest.{FlatSpec, Matchers}

class TrieNodeTest extends FlatSpec with Matchers {

  "add" should "add a word to the trie structure" in {
    val t = new TrieNode
    t.add("hello")
    t.isWord("hello") shouldBe true
  }

}
