package random

import scala.collection.mutable

object PhoneNumber {

  val dictionary: Set[String] = Set("tree", "used", "prat", "snow", "flow")
  val noToChars: Map[Int, List[Char]] =
    Map(2 -> List('a', 'b', 'c'), 3 -> List('d', 'e', 'f'), 4 -> List('g', 'h', 'i'),
        5 -> List('j', 'k', 'l'), 6 -> List('m', 'n', 'o'), 7 -> List('p', 'q', 'r', 's'),
        8 -> List('t', 'u', 'v'), 9 -> List('w', 'x', 'y', 'z'))

  def allWords(num: String): List[String] = {
    var result: List[String] = List.empty[String]
    (0 until num.length).foreach { i =>
      val tempResult: mutable.MutableList[String] = mutable.MutableList.empty[String]
      val ch = noToChars(num(i).asDigit)
      if (result.isEmpty) {
        ch.foreach(c => tempResult += c.toString)
      } else {
        result.foreach { r =>
          ch.foreach(c => tempResult += s"$r$c")
        }
      }
      result = tempResult.toList
    }
    result
  }

//  def getValidWords(num: String): List[String] = {
//    val result: List[String] = List.empty[String]
//    def validWords(index: Int, prefix: String, trie: TrieNode) = {
//      if (index == num.length) {
//        if (trie.isWord(prefix))
//      }
//    }
//  }

}