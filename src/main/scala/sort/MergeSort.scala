package sort

object MergeSort {

  def merge(left: List[Int], right: List[Int]): List[Int] =
    (left, right) match {
      case (Nil, r) => r
      case (l, Nil) => l
      case (l :: lTail, r :: rTail) =>
        if (l < r ) l :: merge(lTail, right) else r :: merge(left, rTail)
    }

  def mergeSort(l: List[Int]): List[Int] = {
    val n = l.length / 2
    if (n == 0) l
    else {
      val (left, right) = l.splitAt(n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

}
