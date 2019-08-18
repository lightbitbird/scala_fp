package pattern.matching

sealed abstract class Tree
case class Branch(value: Int, left: Tree, right: Tree) extends Tree
case object Empty extends Tree

object BinaryTree extends App {
  val tree: Tree = Branch(1, Branch(2, Branch(5, Empty, Branch(3, Empty, Empty)), Empty), Branch(2, Empty, Empty))
  println(BinaryTree.max(tree))
  println(BinaryTree.min(tree))
  println(BinaryTree.depth(tree))

  def max(tree: Tree): Int = tree match {
    case Branch(num, left, right) => {
      val numL = max(left)
      val numR = max(right)
      val list = List(num, numL, numR)
      list.max
    }
    case Empty => 0
  }
  def min(tree: Tree): Int = tree match {
    case Branch(num, left, right) => {
      val numL = min(left)
      val numR = min(right)
      val list = List(num, numL, numR)
      list.min
    }
    case Empty => 0
  }
  def depth(tree: Tree): Int = tree match {
    case Branch(num, left, right) => {
      val depthL = depth(left)
      val depthR = depth(right)
      List(depthL, depthR).max + 1
    }
    case Empty => 0
  }
}
