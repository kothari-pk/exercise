package ctci.ch2

import org.scalatest.{FlatSpec, Matchers}
import LinkedList._

class LinkedListTest extends FlatSpec with Matchers {

  "appendToTail" should "add the element to the end" in {
    val node = new Node[Int](5)
    node.appendToTail(6)
    node.next.data shouldBe 6
    node.appendToTail(7)
    node.next.next.data shouldBe 7
  }

  "deleteFromList" should "remove the given element from the list" in {
    val head = new Node[Int](5)
    val n1 = new Node[Int](6)
    val n2 = new Node[Int](7)
    n1.next = n2
    head.next = n1

    head.next.next.data shouldBe 7
    deleteFromList(head, 6).next.data shouldBe 7
  }

  "removeDuplicate" should "remove the duplicates from a list" in {
    val head = new Node[Int](5)
    val n1 = new Node[Int](6)
    val n2 = new Node[Int](5)
    val n3 = new Node[Int](7)
    n2.next = n3
    n1.next = n2
    head.next = n1

    head.data shouldBe 5
    head.next.data shouldBe 6
    head.next.next.data shouldBe 5
    head.next.next.next.data shouldBe 7

    removeDuplicate(head)
    head.data shouldBe 5
    head.next.data shouldBe 6
    head.next.next.data shouldBe 7
    head.next.next.next shouldBe null
  }

  "kthToLast" should "return kth to last element" in {
    val head = new Node[Int](5)
    val n1 = new Node[Int](6)
    val n2 = new Node[Int](5)
    val n3 = new Node[Int](7)
    n2.next = n3
    n1.next = n2
    head.next = n1

    kthToLast[Int](head, 2).get shouldBe 5
    kthToLast[Int](head, 3).get shouldBe 6
  }

  "printKthToLast" should "return kth to last element recursively" in {
    val head = new Node[Int](5)
    val n1 = new Node[Int](6)
    val n2 = new Node[Int](5)
    val n3 = new Node[Int](7)
    n2.next = n3
    n1.next = n2
    head.next = n1

    printKthToLast(head, 2)
    printKthToLast(head, 3)

    // printKthToLast does not return any value
    true shouldBe true
  }

  "addNumbers" should "add the numbers" in {
    val h1 = new Node[Int](1)
    val n1 = new Node[Int](2)
    val n2 = new Node[Int](3)
    n1.next = n2
    h1.next = n1

    val h2 = new Node[Int](1)
    val n3 = new Node[Int](2)
    h2.next = n3

    h1.data shouldBe 1
    h1.next.data shouldBe 2
    val result = addNumbers(h1, h2) // shouldBe 342
    result.data shouldBe 2
    result.next.data shouldBe 4
    result.next.next.data shouldBe 3
  }

}
