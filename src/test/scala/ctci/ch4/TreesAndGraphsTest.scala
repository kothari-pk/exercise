package ctci.ch4

import org.scalatest.{FlatSpec, Matchers}
import TreesAndGraphs._

class TreesAndGraphsTest extends FlatSpec with Matchers {

  "dfs" should "do a depth first search" in {
    val g = Map(1 -> List(2, 3), 2 -> List(3, 4), 3 -> List(2, 4), 4 -> List(2, 3))
    dfs(g, 1) shouldBe List(1, 3, 4, 2)
  }

  "bfs" should "do a breadth first search" in {
    val g = Map(1 -> List(2, 3), 2 -> List(3, 4), 3 -> List(2, 4), 4 -> List(2, 3))
    bfs(g, 1) shouldBe List(1, 2, 3, 4)
  }

}
