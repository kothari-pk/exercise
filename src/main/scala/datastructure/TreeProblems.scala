package datastructure

case class Tree[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]])

object TreeProblems {

  def inOrder[A](tree: Option[Tree[A]])(f: Tree[A] => Unit): Unit =
    tree match {
      case None => ()
      case Some(t) =>
        if (t.left.isDefined) inOrder(t.left)(f)
        f(t)
        if (t.right.isDefined) inOrder(t.right)(f)
    }

  def preOrder[A](tree: Option[Tree[A]])(f: Tree[A] => Unit): Unit =
    tree match {
      case None => ()
      case Some(t) =>
        f(t)
        if (t.left.isDefined) preOrder(t.left)(f)
        if (t.right.isDefined) preOrder(t.right)(f)
    }

  def postOrder[A](tree: Option[Tree[A]])(f: Tree[A] => Unit): Unit =
    tree match {
      case None => ()
      case Some(t) =>
        if (t.left.isDefined) postOrder(t.left)(f)
        if (t.right.isDefined) postOrder(t.right)(f)
        f(t)
    }

  def breadthFirstSearch[A](tree: Tree[A])(f: Tree[A] => Unit): Unit = {
    def bfs(tree: List[Tree[A]]): Unit = {
      val children: List[Tree[A]] =
        tree.flatMap(t => List(t.left, t.right).flatten)
      children foreach f
      bfs(children)
    }
    bfs(List(tree))
  }

  //         8
  //     4         10
  //  3    5    9      12
  // LCA(3, 5) = 4, LCA(4, 10) = 8, LCA(3, 9) = 8, LCA(3, 4) = 4
  def lowestCommonAncestor[A](
      tree: Tree[Int],
      m: Int,
      n: Int): Option[Tree[Int]] =
    if (tree.value == m || tree.value == n) {
      Some(tree)
    } else {
      val res = (tree.left, tree.right) match {
        case (Some(l), Some(r)) =>
          (lowestCommonAncestor(l, m, n), lowestCommonAncestor(r, m, n))
        case (Some(l), None) => (lowestCommonAncestor(l, m, n), None)
        case (None, Some(r)) => (None, lowestCommonAncestor(r, m, n))
        case (None, None) => (None, None)
      }
      res match {
        case (Some(_), Some(_)) => Some(tree)
        case (None, Some(r)) => Some(r)
        case (Some(l), None) => Some(l)
        case (None, None) => None
      }
    }

  def isBST(tree: Tree[Int], min: Int = Int.MinValue, max: Int = Int.MaxValue): Boolean =
    if (tree.value <= min || tree.value >= max) false
    else {
      (tree.left, tree.right) match {
        case (Some(l), Some(r)) => isBST(l, min, tree.value) && isBST(r, tree.value, max)
        case (Some(l), None) => isBST(l, min, l.value)
        case (None, Some(r)) => isBST(r, r.value, max)
        case (None, None) => true
      }
    }

  def isBalanced[A](tree: Tree[A]): Boolean = {
    def minHeight(t: Tree[A]): Int =
      (t.left, t.right) match {
        case (Some(l), Some(r)) => 1 + Math.min(minHeight(l), minHeight(r))
        case (Some(l), None) => 1 + minHeight(l)
        case (None, Some(r)) => 1 + minHeight(r)
        case (None, None) => 0
      }

    def maxHeight(t: Tree[A]): Int =
      (t.left, t.right) match {
      case (Some(l), Some(r)) => 1 + Math.max(maxHeight(l), maxHeight(r))
      case (Some(l), None) => 1 + maxHeight(l)
      case (None, Some(r)) => 1 + maxHeight(r)
      case (None, None) => 0
    }

    maxHeight(tree) - minHeight(tree) <= 1
  }

  def hasPathSum(tree: Tree[Int], sum: Int): Boolean =
    (tree.left, tree.right) match {
      case (Some(l), Some(r)) => hasPathSum(l, sum - tree.value) || hasPathSum(r, sum - tree.value)
      case (Some(l), None) => hasPathSum(l, sum - tree.value)
      case (None, Some(r)) => hasPathSum(r, sum - tree.value)
      case (None, None) => sum - tree.value == 0
    }

  def isMirror[A](t1: Tree[A], t2: Tree[A]): Boolean =
    (t1.left, t1.right, t2.left, t2.right) match {
      case (Some(t1l), Some(t1r), Some(t2l), Some(t2r)) if t1.value == t2.value => isMirror(t1l, t2r) && isMirror(t2l, t1r)
      case (Some(t1l), None, None, Some(t2r)) if t1.value == t2.value => isMirror(t1l, t2r)
      case (None, Some(t1r), Some(t2l), None) if t1.value == t2.value => isMirror(t2l, t1r)
      case (None, None, None, None) => true
      case _ => false
    }

  class Count { var c: Int = _}
  def kthLargestInBst(bst: Option[Tree[Int]], k: Int, count: Count): Unit =
    bst match {
      case None => ()
      case Some(t) =>
        kthLargestInBst(t.right, k, count)
        count.c += 1
        if (count.c == k) {
          println(s"The kth largest element is ${t.value}")
          return ()
        }
        kthLargestInBst(t.left, k, count)
    }

}

object TreeProblemsTest extends App {
  import TreeProblems._

  override def main(args: Array[String]): Unit = {
    val bst: Tree[Int] = Tree[Int](
      10,
      Some(
        Tree[Int](
          5,
          Some(Tree[Int](1, None, None)),
          Some(Tree[Int](7, None, None)))),
      Some(
        Tree[Int](
          15,
          Some(Tree[Int](12, None, None)),
          Some(Tree[Int](20, None, None))))
    )

    println(lowestCommonAncestor[Int](bst, 1, 12).get.value)
    println(isBST(bst))
    println(isBalanced(bst))
    println(hasPathSum(bst, 16))
    kthLargestInBst(Some(bst), 3, new Count)
  }
}
