package random

object Random {

  // Duplicate without extra space
  def dupNoExtraSpace(arr: Array[Int]): Unit = {
    (0 until arr.length).foreach { i =>
      // Check if the element is already seen
      if (arr(Math.abs(arr(i))) >= 0) {
        // Mark the element seen
        arr(Math.abs(arr(i))) = - arr(Math.abs(arr(i)))
      } else println(s"Duplicate element: ${Math.abs(arr(i))}")
    }
  }

  // MIN missing positive number (array only has positive number)
  // If the array has negative number then divide the array in positives and negatives
  // Constraint: Use no extra place
  // Trick: Use the index and the incrementing by 1 numbers
  def minMissingNumber(arr: Array[Int]): Int = {
    val size = arr.length
    (0 until size).foreach { i =>
      if(Math.abs(arr(i)) - 1 < size && arr(Math.abs(arr(i)) - 1) > 0) {
        // Mark the element at location i -ve
        arr(Math.abs(arr(i)) - 1) = -arr(Math.abs(arr(i)) - 1)
      }
    }

    // return the first non negative index
    (0 until size).foreach { i =>
      if (arr(i) > 0) return i + 1
    }
    -1
  }

  def productArray(arr: Array[Int]): Array[Int] = {
    // Create to arrays one product from left and other product right
    val size = arr.length
    val left: Array[Int] = Array.fill[Int](size)(1)
    val right: Array[Int] = Array.fill[Int](size)(1)
    val result: Array[Int] = Array.fill[Int](size)(1)

    // First element for left and last element for right should be 1
    // Fill the left array. Multiplying all the elements to the left
    (1 until size).foreach { i =>
      left(i) = arr(i - 1) * left(i - 1)
    }

    (size - 2 to 0 by - 1).foreach { i =>
      right(i) = arr(i + 1) * right (i + 1)
    }

    (0 until size).foreach { i =>
      result(i) = left(i) * right(i)
    }
    result
  }

  // With O(1) extra space
  def productArray1(arr: Array[Int]): Array[Int] = {
    // Create to arrays one product from left and other product right
    val size = arr.length
    var temp = 1
    val result: Array[Int] = Array.fill[Int](size)(1)

    // First element for left and last element for right should be 1
    // Fill the left array. Multiplying all the elements to the left
    (0 until size).foreach { i =>
      result(i) = temp
      temp *= arr(i)
    }

    // Reset temp to 1
    temp = 1

    (size - 1 to 0 by - 1).foreach { i =>
      result(i) *= temp
      temp *= arr(i)
    }

    result
  }

}
