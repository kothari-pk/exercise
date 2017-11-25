package random

object DP {

  val minSquaresCache: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map.empty[Int, Int]

  // Memoization DP solution
  def getMinSquares(n: Int): Int = {
    // base case
    if(n < 3) n
    else {
      // Max numbers required is n
      // i.e. (1*1 + 1*1 + ...)
      var res = n
      // Go through the smaller numbers to find the min
      (1 to n).foreach { i =>
        val temp = i * i
        if (temp > n) return res
        else {
          // check if we have already solved this
          minSquaresCache.get(n - temp) match {
            case Some(r) => res = Math.min(res, 1 + r)
            case None =>
              val r = getMinSquares(n - temp)
              minSquaresCache(n - temp) =  r
              res = Math.min(res, 1 + r)
          }
        }
      }
      res
    }
  }

  // Bottom Up DP solution
  def getMinSquaresBU(n: Int): Int = {
    // Create an array for all numbers 0 to n (including n)
    val dp: Array[Int] = Array.fill[Int](n + 1)(0)
    // Initialize for the base cases
    dp(0) = 0
    dp(1) = 1
    dp(2) = 2
    dp(3) = 3

    (4 to n).foreach { i =>
      // Max numbers required is n
      // i.e. (1*1 + 1*1 + ...)
      dp(i) = i
      // Go through all the smaller numbers to find the min
      (1 to i).foreach { j =>
        val temp = j * j
        if (temp < i) dp(i) = Math.min(dp(i), 1 + dp(i - temp))
      }
    }
    dp(n)
  }

  // Coin change DP solution
  val coinChangeMemo: scala.collection.mutable.Map[String, Long] = scala.collection.mutable.Map.empty[String, Long]

  def count(denominations: Array[Int], coinCount: Int, n: Int): Long = {
    if(n < 0) 0
    else if(n == 0) 1
    else if (coinCount <=0 && n >= 1) 0
    else {
      coinChangeMemo.get(s"$n-$coinCount") match {
        case Some(r) => r
        case None =>
          val res = count(denominations, coinCount - 1, n) +
              count(denominations, coinCount, n - denominations(coinCount - 1))
          coinChangeMemo(s"$n-$coinCount") = res
          res
      }
    }
  }

  def min(x: Int, y: Int, z: Int): Int =
    if (x < y && x < z) x else if (y < x && y < z) y else z

  def editDist(s1: String, s2: String, m: Int, n: Int): Int =
    if (m == 0) n
    else if (n == 0) m
    else if (s1(m - 1) == s2(n - 1)) editDist(s1, s2, m - 1, n - 1)
    else 1 + min(editDist(s1, s2, m, n - 1), editDist(s1, s2, m - 1, n), editDist(s1, s2, m - 1, n - 1))

  def editDistDP(s1: String, s2: String, m: Int, n: Int): Int = {
    val res: Array[Array[Int]] = Array.ofDim(m + 1, n + 1)
    (0 to m).foreach { i =>
      (0 to n).foreach { j =>
        // If first string is empty, only option is to
        // insert all characters of second string
        if(i == 0) res(i)(j) = j
        // If second string is empty, only option is to
        // remove all characters of second string
        else if(j == 0) res(i)(j) = i
        // If last characters are same, ignore last char
        // and recur for remaining string
        else if(s1(i - 1) == s2(j - 1)) res(i)(j) = res(i - 1)(j -1)
        // If last character are different, consider all
        // possibilities and find minimum
        else res(i)(j) = 1 + min(res(i)(j - 1), res(i - 1)(j), res(i - 1)(j - 1))
      }
    }
    res(m)(n)
  }

  val printCountMemo: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map.empty[Int, Int]
  def printCountRecDP(n: Int): Int = {
    if (n < 0) 0
    else if (n == 0) 1
    else {
      printCountMemo.get(n) match {
        case Some(r) => r
        case None =>
          val res = printCountRecDP(n - 1) + printCountRecDP(n - 2) + printCountRecDP(n - 3)
          printCountMemo(n) = res
          res
      }
    }
  }

  def maxSubArraySumDP(arr: Array[Int]): Int = {
    var currMax: Int = arr(0)
    var maxSoFar: Int = arr(0)

    (1 until arr.length).foreach { i =>
      currMax = Math.max(arr(i), currMax + arr(i))
      maxSoFar = Math.max(maxSoFar, currMax)
    }
    maxSoFar
  }

  // Longest increasing Subsequence
  // LIS for {10, 22, 9, 33, 21, 50, 41, 60, 80} is 6 and LIS is {10, 22, 33, 50, 60, 80}
  //      i
  // 10, 22, 9, 33, 21, 50, 41, 60 , 80
  // j
  // 1 , 1 , 1, 1 , 1 , 1,  1,  1,   1
  def lis(arr: Array[Int]): Int = {
    val length = arr.length
    val lis: Array[Int] = Array.fill[Int](length)(1)
    (1 until length).foreach { i =>
      (0 until i).foreach { j =>
        if (arr(i) > arr(j) && lis(i) < lis(j) + 1)
          lis(i) = lis(j) + 1
      }
    }
    lis.max
  }

}
