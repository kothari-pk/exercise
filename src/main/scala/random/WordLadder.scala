package random

/**
  * Given:
  * beginWord = "hit"
  * endWord = "cog"
  * wordList = ["hot","dot","dog","lot","log","cog"]
  * As one shortest transformation is "hit" -> "hot" -> "dot" -> "dog" -> "cog",
  * return its length 5.
  */
object WordLadder {

  /**
    * @param word the word from the wordList
    * @param len minimum chain length to reach this word
    */
  case class Item(word: String, len: Int)

  // Check if the 2 strings differ by only 1 character
  def isAdjacent(s1: String, s2: String): Boolean = {
    var count: Int = 0
    (0 until s1.size).foreach { i =>
      if(s1(i) != s2(i)) count += 1
      if(count > 1) return false
    }
    count == 1
  }

  def countWordLadder(beginWord: String, endWord: String, wordList: Set[String]): Int = {
    val queue: scala.collection.mutable.Queue[Item] = scala.collection.mutable.Queue.empty[Item]
    queue.enqueue(Item(beginWord, 1))
    while (queue.nonEmpty) {
      val current: Item = queue.dequeue
      // Loop through all the words in the wordList
      wordList.foreach { w =>
        // Check if there is a word with only 1 different character
        if(isAdjacent(current.word, w)) {
          // add it to the queue
          queue.enqueue(Item(w, current.len + 1))
          if (current.word == endWord) return current.len
        }
      }
    }
    0
  }

}
