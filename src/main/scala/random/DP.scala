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

}
