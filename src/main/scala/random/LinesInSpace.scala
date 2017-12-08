package random

object LinesInSpace {

  // Find the max points on a lines in a 2 D plane of points
  // Input are coordinates (x, y)
  // Solution:
  // TIP: To make a line we need 2 points
  // Well we can make a line with any 2 points.
  // The question is does 3 points lie on the same line
  // What is the equation that represents a line?
  //       y = mx + c
  // where (x, y) are the coordinates. c is the constant and m is the slope
  // if 2 points are on the same line then:
  // y1 = mx1 + c
  // y2 = mx2 + c
  // y2 - y1 = (mx2 + c) - (mx1 + c)
  // y2 - y1 = m(x2 - x1)
  // So the slope m = (y2 - y1)/(x2 - x1)
  // if a 3rd point(x3, y3) is also on the same line then
  // (y3 - y1)/(x3 - x1) = (y2 - y1)/(x2 - x1) = m
  // Now this can be a decimal and we loose precision, either it is ok
  // or we have to treat this as a rational number

  // Now lets implement
  def maxPoints(points: Vector[(Int, Int)]): Int = {
    val size = points.length
    var maxPoint = 0
    var currMaxPoints = 0
    var overlappingPoints = 0
    if (size == 0) 0
    else if(size <= 2) 1
    else {
      // Map to store the slope to count
      var slopeToCount: Map[(Int, Int), Int] = Map.empty[(Int, Int), Int]
      (0 until size).foreach { i =>
        currMaxPoints = 0
        overlappingPoints = 0
        (i + 1 until size).foreach { j =>
          // Check for overlapping points
          if (points(i) == points(j)) overlappingPoints += 1
          else {
            var yDiff = points(j)._2 - points(i)._2
            var xDiff = points(j)._1 - points(i)._1

            val g = gcd(xDiff, yDiff)

            // Reduce the difference with the gcd
            yDiff /= g
            xDiff /= g
            // Now update the slopeToCount map
            val c = slopeToCount.getOrElse((yDiff, xDiff), 0)
            slopeToCount += (yDiff, xDiff) -> (c + 1)
            currMaxPoints = Math.max(currMaxPoints, slopeToCount.getOrElse((yDiff, xDiff), 0))
          }
        }
        maxPoint = Math.max(maxPoint, currMaxPoints + overlappingPoints + 1)
      }
    }
    maxPoint
  }

  def gcd(x: Int, y: Int): Int =
    if(y == 0) x
    else gcd(y, x % y)

}
