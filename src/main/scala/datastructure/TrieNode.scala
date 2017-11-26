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

class TrieNode2(val prefix: String) {
  val children: scala.collection.mutable.Map[Char, TrieNode2] = scala.collection.mutable.Map.empty[Char, TrieNode2]
  var isEnd: Boolean = false

  def add(word: String): Unit = word.headOption match {
    case None => isEnd = true
    case Some(l) => children.getOrElseUpdate(l, new TrieNode2(s"$prefix$l")).add(word.substring(1))
  }

  def isWord(s: String): Boolean = s.headOption match {
    case None => isEnd
    case Some(l) => children.get(l) match {
      case None => false
      case Some(t) => t.isWord(s.substring(1))
    }
  }

  def getWordsForPrefix(p: String): List[String] = {
    if (p == prefix) findAllChildrenWords(this)
    else {
      children.values.toList.find(_.prefix == p) match {
        case Some(t) => findAllChildrenWords(t)
        case None => List.empty[String]
      }
    }
  }

  def findAllChildrenWords(n: TrieNode2, result: List[String] = List.empty[String]): List[String] =
  if (n.children.isEmpty) result :+ n.prefix
  else {
    val r = if (n.isEnd) result :+ n.prefix else result
    n.children.values.toList.flatMap(x => findAllChildrenWords(x, r))
  }

}