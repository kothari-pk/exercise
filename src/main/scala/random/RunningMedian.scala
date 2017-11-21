package random

import scala.collection.mutable._


object MinValue extends Ordering[Int] {
  def compare(x: Int, y: Int): Int = y compare x
}

object RunningMedian {

  def add(num: Int, lowerHalf: PriorityQueue[Int], upperHalf: PriorityQueue[Int]): Unit =
    if (lowerHalf.isEmpty || num < lowerHalf.head)
      lowerHalf.enqueue(num)
    else upperHalf.enqueue(num)

  def reBalance(lowerHalf: PriorityQueue[Int], upperHalf: PriorityQueue[Int]): Unit = {
    val (biggerHeap, smallerHeap) =
      if (lowerHalf.size > upperHalf.size)
        (lowerHalf, upperHalf)
      else (upperHalf, lowerHalf)

    if (biggerHeap.size - smallerHeap.size >= 2) smallerHeap.enqueue(biggerHeap.dequeue)
  }

  def getMedian(lowerHalf: PriorityQueue[Int], upperHalf: PriorityQueue[Int]): Double = {
    val (biggerHeap, smallerHeap) =
      if (lowerHalf.size > upperHalf.size)
        (lowerHalf, upperHalf)
      else (upperHalf, lowerHalf)

    if (biggerHeap.size == smallerHeap.size) {
      (biggerHeap.head.toDouble + smallerHeap.head) / 2
    } else biggerHeap.head
  }

  def getMedians(arr: Array[Int]): Array[Double] = {
    // MaxHeap
    val lowerHalf: PriorityQueue[Int] = PriorityQueue.empty[Int]
    // MinHeap
    val upperHalf: PriorityQueue[Int] = PriorityQueue.empty(MinValue)
    val medians: Array[Double] = Array.fill[Double](arr.length)(0)
    (0 until arr.length).foreach { i =>
      add(arr(i), lowerHalf, upperHalf)
      reBalance(lowerHalf, upperHalf)
      medians(i) = getMedian(lowerHalf, upperHalf)
    }
    medians
  }

}
