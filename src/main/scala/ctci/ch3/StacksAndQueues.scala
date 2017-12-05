package ctci.ch3

import scala.collection.mutable

class NodeWithMin(val data: Int) {
  var min: Int = _
}

class StackWithMin extends mutable.Stack[NodeWithMin] {
  def push(v: Int): Unit = {
    val newMin = Math.min(v, min())
    super.push(new NodeWithMin(v) { min = newMin })
  }

  def min(): Int =
    if (this.isEmpty) Int.MaxValue else top.min
}

class StackWithMin2 extends mutable.Stack[Int] {
  val minStack: mutable.Stack[Int] = new mutable.Stack[Int]

  override def push(v: Int): this.type = {
    if (v <= min) minStack.push(v)
    super.push(v)
  }

  override def pop: Int = {
    val r = super.pop
    if (r == min) minStack.pop
    r
  }

  def min: Int = if (minStack.isEmpty) Int.MaxValue else minStack.top
}

class QueueWithStack[A](val data: A) {
  val enqueueStack: mutable.Stack[A] = new mutable.Stack[A]
  val dequeueStack: mutable.Stack[A] = new mutable.Stack[A]

  def enqueue(d: A): Unit = enqueueStack.push(d)

  def dequeue: A = {
    if (dequeueStack.isEmpty) {
      while(enqueueStack.nonEmpty) {
        dequeueStack.push(enqueueStack.pop())
      }
    }
    dequeueStack.pop()
  }
}

object StacksAndQueues {
  def sortStack(s: mutable.Stack[Int]): mutable.Stack[Int] = {
    val sortedStack: mutable.Stack[Int] = new mutable.Stack[Int]

    while (s.nonEmpty) {
      val curr = s.pop()
      if (sortedStack.isEmpty || curr <= sortedStack.top)
        sortedStack.push(curr)
      else {
        while (curr > sortedStack.top || sortedStack.isEmpty) {
          s.push(sortedStack.pop())
        }
        sortedStack.push(curr)
      }
    }
    sortedStack
  }
}
