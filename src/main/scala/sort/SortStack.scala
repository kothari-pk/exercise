package sort

import scala.collection.mutable

object SortStack {

  def stackSort(stack: mutable.Stack[Int]): mutable.Stack[Int] = {
    // create a temp stack
    val tmpStack: mutable.Stack[Int] = mutable.Stack[Int]()
    while (stack.nonEmpty) {
      val t = stack.pop()
      while (tmpStack.nonEmpty && t > tmpStack.head) {
        stack.push(tmpStack.pop)
      }
      tmpStack.push(t)
    }
    tmpStack
  }

}
