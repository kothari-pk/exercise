package matrix

object SpiralOrder {

  def spiralOrder(m: Array[Array[Int]]): List[Int] = {
    val res: scala.collection.mutable.MutableList[Int] = scala.collection.mutable.MutableList.empty[Int]

    var rowBegin = 0
    var rowEnd = m.length - 1
    var colBegin = 0
    var colEnd = m(0).length - 1

    while (rowBegin <= rowEnd && colBegin <= colEnd) {
      // Traverse right
      (colBegin to colEnd).foreach { c =>
        res += m(rowBegin)(c)
      }
      rowBegin += 1

      // Traverse down
      (rowBegin to rowEnd).foreach { r =>
        res += m(r)(colEnd)
      }
      colEnd -= 1

      // Traverse Left. BUT, only if rowBegin <= rowEnd
      if (rowBegin <= rowEnd) {
        (colEnd to colBegin by -1).foreach { c =>
          res += m(rowEnd)(c)
        }
      }
      rowEnd -= 1

      // Traverse Up. BUT, only if colBegin <= colEnd
      if (colBegin <= colEnd) {
        (rowEnd to rowBegin by -1).foreach { r =>
          res += m(r)(colBegin)
        }
      }
      colBegin += 1
    }

    res.toList
  }

}
