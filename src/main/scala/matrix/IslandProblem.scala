package matrix

object IslandProblem {

  val Row: Int = 5
  val Coll: Int = 5

  def countIslands(m: Array[Array[Int]]): Int = {
    var count = 0
    val visited: Array[Array[Boolean]] = Array.fill[Boolean](Row, Coll)(false)

    for (r <- 0 until Row) {
      for (c <- 0 until Coll) {
        if (!visited(r)(c) && m(r)(c) == 1) {
          // search
          search(m, visited, r, c)
          count += 1
        }
      }
    }

    count
  }

  def search(m: Array[Array[Int]], visited: Array[Array[Boolean]], currRow: Int, currCol: Int): Unit = {
    val row = Array[Int](-1, -1, -1, 0, 0, 1, 1, 1)
    val col = Array[Int](-1, 0, 1, -1, 1, -1, 0, 1)

    visited(currRow)(currCol) = true

    for (i <- 0 until 8) {
      if (canVisit(m, visited, currRow + row(i), currCol + col(i)))
        search(m, visited, currRow + row(i), currCol + col(i))
    }
  }

  def canVisit(m: Array[Array[Int]], visited: Array[Array[Boolean]], currRow: Int, currCol: Int): Boolean =
  currRow >= 0 && currRow < Row && currCol >= 0 && currCol < Coll && !visited(currRow)(currCol) && m(currRow)(currCol) == 1

}
