package random

import scala.collection.mutable

object CommonTime extends App {

  case class Point(time: Int, isStart: Boolean)

  // p1 -> (10, 20) (40, 50) (70, 80)
  // p2 -> (15, 25) (30, 48)
  // Answer (40 - 48) for 8 mins
  // 10s, 20e, 40s, 50e, 70s, 80e, 15s, 25e, 30s, 48e
  // 10s, 15s, 20e, 25e, 30s, 40s, 48e, 50e, 70s, 80e
  def meetingTime(p1: Array[(Int, Int)], p2: Array[(Int, Int)]): Int = {
    val s1 = p1.length
    val s2 = p2.length
    // First convert both the timeslots to points
    val point1: mutable.MutableList[Point] = mutable.MutableList.empty[Point]
    val point2: mutable.MutableList[Point] = mutable.MutableList.empty[Point]

    (0 until s1).foreach { i =>
      point1 += Point(p1(i)._1, true)
      point1 += Point(p1(i)._2, false)
    }

    (0 until s2).foreach { i =>
      point2 += Point(p2(i)._1, true)
      point2 += Point(p2(i)._2, false)
    }

    // Add both the arrays
    val points = (point1 ++ point2).sortBy(_.time)
    var previous: Point = points.head
    var previousStartCount: Int = 1
    var max = -1
    points.tail.foreach { curr =>
      if (previous.isStart && !curr.isStart && previousStartCount >= 2) {
        max = Math.max(max, curr.time - previous.time)
      }
      else if(previous.isStart && curr.isStart) previousStartCount += 1
      else if(!previous.isStart && curr.isStart) previousStartCount += 1
      else previousStartCount = 0
      previous = curr
    }
    max
  }

  override def main(args: Array[String]): Unit = {
    println(meetingTime(Array((10, 20), (40, 50), (70, 80)), Array((15, 25), (30, 48))))
  }

}
