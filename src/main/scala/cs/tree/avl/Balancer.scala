package cs.tree.avl

import scala.math.max

trait Balancer {

  private[avl]def doBalance(n:Node): Node = {
    n.height = totalHeight(n)
    balanceFactor(n) match {
      case b if b < -1 =>
        if (balanceFactor(n.right) > 0) {
          n.right = Some(rotateRight(n.right.get))
        }
        rotateLeft(n)
      case b if b > 1 =>
        if (balanceFactor(n.left) < 0) {
          n.left = Some(rotateLeft(n.left.get))
        }
        rotateRight(n)
      case _ => n
    }
  }

  private[avl] def rotateRight(parent: Node): Node = {
    val newParent = parent.left.get //left exist because balance factor > 0
    parent.left = newParent.right
    newParent.right = Some(parent)
    parent.height = totalHeight(parent)
    newParent.height = totalHeight(newParent)
    newParent
  }

  private[avl] def rotateLeft(parent: Node): Node = {
    val newParent = parent.right.get //right exist because balance factor < 0
    parent.right = newParent.left
    newParent.left = Some(parent)
    parent.height = totalHeight(parent)
    newParent.height = totalHeight(newParent)
    newParent
  }

  private[avl] def balanceFactor(n: Node):Int = height(n.left) - height(n.right)

  private[avl] def balanceFactor(node: Option[Node]): Int = node match {
    case None => 0
    case Some(n) => balanceFactor(n)
  }

  private[avl] def totalHeight(node: Node)= 1 + max(height(node.left), height(node.right))

  private[avl] def height(node: Option[Node]): Int =
    node match {
      case None => 0
      case Some(n) => n.height
    }

  //avl invariant
  def isBalanced(node: Option[Node]): Boolean = {
    node match {
      case None => true
      case Some(n) =>
        val b = balanceFactor(n)
        if (b < -1 || b > 1) false
        else isBalanced(n.left) && isBalanced(n.right)
    }
  }

}
