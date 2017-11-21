package random

object Intersection {

  def findIntersectUnsorted(arr1: Array[Int], arr2: Array[Int]): Set[Int] = {
    // Convert the second array to a set
    val set = arr2.toSet
    var result = Set.empty[Int]
    (0 until arr1.length).foreach { i =>
      val v = arr1(i)
      if(set.contains(v)) result = result + v
    }
    result
  }

  def findIntersectSorted(arr1: Array[Int], arr2: Array[Int]): Set[Int] = {
    var result = Set.empty[Int]
    val m = arr1.length
    val n = arr2.length
    var i = 0
    var j = 0
    while (i < m && j < n) {
      if(arr1(i) < arr2(j)) {
        i += 1
      } else if (arr1(i) == arr2(j)) {
        result = result + arr1(i)
        i += 1
        j += 1
      } else {
        j += 1
      }
    }
    result
  }

}
