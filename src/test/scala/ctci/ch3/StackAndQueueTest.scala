package ctci.ch3

import org.scalatest.{FlatSpec, Matchers}
import StacksAndQueues._

class StackAndQueueTest extends FlatSpec with Matchers {
  "sortStack" should "sort the stack" in {
    val s: scala.collection.mutable.Stack[Int] = scala.collection.mutable.Stack[Int](8, 1, 4, 5)

    sortStack(s) shouldBe scala.collection.mutable.Stack[Int](1, 4, 5, 8)
  }

  it should "sort the stack with duplicates" in {
    val s: scala.collection.mutable.Stack[Int] = scala.collection.mutable.Stack[Int](8, 1, 4, 1, 5)

    sortStack(s) shouldBe scala.collection.mutable.Stack[Int](1, 1, 4, 5, 8)
  }
}
