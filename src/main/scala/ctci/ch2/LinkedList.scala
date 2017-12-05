package ctci.ch2

class Node[A](val data: A) {
  var next: Node[A] = _

  def appendToTail(v: A): Unit = {
    // Create the node
    val end = new Node[A](v)
    var n = this
    while (n.next != null) {
      n = n.next
    }
    n.next = end
  }
}

object LinkedList {

  def deleteFromList[A](head: Node[A], d: A): Node[A] = {
    var n = head
    if (n.data == d) {
      return head.next
    }

    while (n.next != null) {
      if (n.next.data == d) {
        n.next = n.next.next
        return n
      }
      n = n.next
    }
    n
  }

  def removeDuplicate(head: Node[Int]): Unit = {
    val temp: scala.collection.mutable.HashSet[Int] = scala.collection.mutable.HashSet.empty[Int]
    var previous: Node[Int] = null
    var n = head
    while (n != null) {
      if (temp.contains(n.data)) previous.next = n.next
      else {
        temp += n.data
        previous = n
      }
      n = n.next
    }
  }

  def kthToLast[A](head: Node[A], k: Int): Option[A] = {
    type Index = Int
    val m: scala.collection.mutable.HashMap[Index, A] = scala.collection.mutable.HashMap.empty[Index, A]

    var n = head
    var index = 0
    while (n != null) {
      m += (index -> n.data)
      index += 1
      n = n.next
    }
    m.get(index - k)
  }

  def printKthToLast(head: Node[Int], k: Int): Int =
    if (head == null) 0
    else {
      val index = printKthToLast(head.next, k) + 1
      if (k == index) println(s"$k th tp last node is: ${head.data}")
      index
    }

  def addNumbers(l1: Node[Int], l2: Node[Int], carry: Int = 0): Node[Int] =
    if (l1 == null && l2 == null && carry == 0) null
    else {
      var value = carry
      if (l1 != null) value += l1.data
      if (l2 != null) value += l2.data
      val result = new Node[Int](value % 10)
      val c = value / 10
      val more = if (l1 != null && l2 != null)
        addNumbers(l1.next, l2.next, c)
        else if (l1 != null && l2 == null)
        addNumbers(l1.next, null, c)
        else if (l1 == null && l2 != null)
        addNumbers(null, l2.next, c)
        else addNumbers(null, null, c)
      result.next = more
      result
    }
}
