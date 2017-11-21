package random

object WordBreak {

  val dictionary =
    Set("mobile", "samsung", "sam", "sung", "man", "mango", "icecream", "and", "go", "i", "like", "likes", "ice", "cream")

  def isTextWords(str: String): Boolean = {
    val size = str.length
    // create another array to store the results at positions
    // The final result is stored at the final position in this array
    // -1 mean no word ending at this location. 0 means there is a
    // word ending at this position
    val pos: Array[Int] = Array.fill[Int](size + 1)(-1)
    // Mark the first index as a word
    pos(0) = 0
    (0 until size).foreach { i =>
      // make the next loop only if there is a word ending at this position
      if(pos(i) == 0) {
        (i + 1 to size).foreach { j =>
          if(dictionary.contains(str.substring(i, j)))
            pos(j) = 0
        }
      }
    }
    pos(size) == 0
  }

}
