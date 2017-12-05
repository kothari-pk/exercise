package ctci.ch1

object StringAndArray {

  // Functional solution with extra space of using a Map
  def isAllUniqueFunctional(str: String): Boolean =
    !str.groupBy(identity).mapValues(_.size).exists(_._2 > 1)

  def isAllUnique(str: String): Boolean = {
    val temp: scala.collection.mutable.Map[Char, Int] = scala.collection.mutable.Map.empty[Char, Int]
    str.foreach { c =>
      temp.get(c) match {
        case Some(v) => temp += (c -> (v + 1))
        case None => temp += (c -> 1)
      }
    }
    !temp.exists(_._2 > 1)
  }

  // This is an O(n^2) solution
  // If we assume the chars are 'a' to 'z' (only lowercase) we can sort them
  // and check linearly if the pairs are the same
  def isAllUniqueNoExtraSpace(str: String): Boolean = {
    val size = str.length
    (0 until size).foreach { i =>
      (i + 1 until size).foreach { j =>
        if (str(i) == str(j)) return false
      }
    }
    true
  }

  def isAllUniqueWithSort(str: String): Boolean = {
    val sortedStr = str.sorted
    (0 until sortedStr.size - 1).foreach { i =>
      if (sortedStr(i) == sortedStr(i + 1)) return false
    }
    true
  }

  def isPermutation(s1: String, s2: String): Boolean = {
    val m1 = s1.groupBy(identity).mapValues(_.length)
    val m2 = s2.groupBy(identity).mapValues(_.length)

    m1 == m2
  }

  def urlify(str: String, length: Int): String = {
    val result: Array[Char] = new Array[Char](str.length)
    var j = 0
    (0 until length).foreach { i =>
      if(str(i) == ' ') {
        result(j) = '%'
        result(j + 1) = '2'
        result(j + 2) = '0'
        j = j + 3
      } else {
        result(j) = str(i)
        j = j + 1
      }
    }
    result.mkString
  }

  def urlifyNoExtraSpace(str: String, length: Int): String = {
    val ch: Array[Char] = str.toCharArray
    var spaceCount = 0
    (0 until length).foreach { i =>
      if(ch(i) == ' ') spaceCount += 1
    }

    var index = length + spaceCount * 2

    (length - 1 to 0 by -1).foreach { i =>
      if (ch(i) == ' ') {
        ch(index - 1) = '0'
        ch(index - 2) = '2'
        ch(index - 3) = '%'
        index = index - 3
      } else {
        ch(index - 1) = ch(i)
        index -= 1
      }
    }
    ch.mkString
  }

  def palindromePerm(str: String): Boolean = {
    var lengthNoSpace = 0
    val temp: scala.collection.mutable.Map[Char, Int] = scala.collection.mutable.Map.empty[Char, Int]
    str.foreach { s =>
      if (s != ' ') {
        lengthNoSpace += 1
        temp.get(s) match {
          case Some(v) => temp += (s -> (v + 1))
          case None => temp += (s -> 1)
        }
      }
    }
    // if the length is even then all values must be 2
    // else if length is odd then n - 1 values must be 2 and 1 value must be 1
    if (lengthNoSpace % 2 == 0)
      temp.forall(_._2 == 2)
    else {
      var ones = 0
      temp.foreach { t =>
        if(t._2 == 2) ()
        else if (t._2 == 1) ones += 1
        else return false
      }
      ones == 1
    }
  }

  private def oneReplace(str1: String, str2: String, l: Int): Boolean = {
    var singleReplace: Boolean = false
    (0 until l).foreach { i =>
      if(str1(i) != str2(i) && singleReplace) return false
      else if(str1(i) != str2(i)) singleReplace = true
    }
    true
  }

  private def oneAdd(str1: String, str2: String): Boolean = {
    var i = 0
    var j = 0
    while (i < str1.length && j < str2.length) {
      if(str1(i) != str2(j)) {
        if (i != j) return false
        i += 1
      } else {
        i += 1
        j += 1
      }
    }
    true
  }

  def oneAway(s1: String, s2: String): Boolean = {
    val l1 = s1.length
    val l2 = s2.length
    // If the lengths are equal only possible operation is modify
    if (l1 == l2) oneReplace(s1, s2, l1)
    else if (l1 - l2 == 1) oneAdd(s1, s2)
    else if (l2 - l1 == 1) oneAdd(s2, s1)
    else false
  }

  def compress(str: String): String = {
    var temp: Char = str(0)
    var count = 1
    val result: StringBuilder = StringBuilder.newBuilder
    (1 until str.length).foreach { i =>
      if (str(i) == temp) count += 1
      else {
        result.append(s"$temp$count")
        temp = str(i)
        count = 1
      }
    }
    result.append(s"$temp$count").toString
  }

  def zeroMatrix(m: Array[Array[Int]]): Array[Array[Int]] = {
    val rowsWithZero: scala.collection.mutable.MutableList[Int] = scala.collection.mutable.MutableList.empty[Int]
    val colWithZero: scala.collection.mutable.MutableList[Int] = scala.collection.mutable.MutableList.empty[Int]

    (0 until m.length).foreach { r =>
      (0 until m(0).length).foreach { c =>
        if (m(r)(c) == 0) {
          rowsWithZero += r
          colWithZero += c
        }
      }
    }

    rowsWithZero.foreach { r =>
      (0 until m(0).length).foreach { c =>
        m(r)(c) = 0
      }
    }

    colWithZero.foreach { c =>
      (0 until m.length).foreach { r =>
        m(r)(c) = 0
      }
    }
    m
  }
}
