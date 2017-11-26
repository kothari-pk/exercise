package datastructure

// Implementation of the Trie data structure
// Usage: Auto complete, word search

// Each trie node can only contains 'a'-'z' characters. So we can use a small array to store the character.
class TrieNode {
  val children: Array[TrieNode] = new Array[TrieNode](26)
  var isEnd: Boolean            = false

  def add(word: String): Unit = word.headOption match {
    case None => isEnd = true
    case Some(l) =>
      val index = l - 'a'
      if (children(index) == null) children(index) = new TrieNode
      children(index).add(word.substring(1))
  }

  def isWord(s: String): Boolean = s.headOption match {
    case None => isEnd
    case Some(l) =>
      val index = l - 'a'
      if (children(index) == null)
        false
      else children(index).isWord(s.substring(1))
  }

  def startsWith(s: String): Boolean = s.headOption match {
    case None => true
    case Some(l) =>
      val index = l - 'a'
      if (children(index) == null)
        false
      else children(index).startsWith(s.substring(1))
  }
}
