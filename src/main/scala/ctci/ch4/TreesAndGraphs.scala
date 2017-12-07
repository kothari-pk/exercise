package ctci.ch4

import scala.collection.mutable

case class Tree[A](data: A, left: Option[Tree[A]], right: Option[Tree[A]])

object TreesAndGraphs {

  type Vertex = Int
  // Graph can also be represented as an adjacency matrix
  type Graph = Map[Vertex, List[Vertex]]

  def dfs(g: Graph, start: Vertex): List[Vertex] = {
    val visited: mutable.MutableList[Vertex] = mutable.MutableList.empty[Vertex]
    val stack: mutable.Stack[Vertex] = mutable.Stack[Vertex]()
    var current = start

    stack.push(current)
    while (stack.nonEmpty) {
      current = stack.pop
      if (!visited.contains(current)) {
        visited += current
        g.get(current).foreach(_.foreach(stack.push))
      }
    }
    visited.toList
  }

  def bfs(g: Graph, start: Vertex): List[Vertex] = {
    val visited: mutable.MutableList[Vertex] = mutable.MutableList.empty[Vertex]
    val queue: mutable.Queue[Vertex] = mutable.Queue[Vertex]()
    var current = start

    queue.enqueue(current)
    while (queue.nonEmpty) {
      current = queue.dequeue
      if (!visited.contains(current)) {
        visited += current
        g.get(current).foreach(_.foreach(queue.enqueue(_)))
      }
    }
    visited.toList
  }

}
