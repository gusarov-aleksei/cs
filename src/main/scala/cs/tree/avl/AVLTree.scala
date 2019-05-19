package cs.tree.avl

import scala.annotation.tailrec

/*
  Self-balancing binary tree (AVL tree)
 */
class AVLTree extends Balancer {

  private var _root: Option[Node] = None

  private[avl] def root = _root

  def get(key: Int):  Option[Node] = get(_root, key)

  private[avl] def get(node: Option[Node], key: Int): Option[Node] = node match {
    case None => None
    case Some(n) =>
      if (key < n.key) get(n.left, key)
      else if (key > n.key) get(n.right, key)
      else node
  }

  def contains(key: Int): Boolean = get(key).isDefined

  private[avl] def contains(node: Option[Node] , key: Int): Boolean = get(node, key).isDefined

  def putAll(keys: Seq[Int]): Unit = {
      keys.foreach(k => put(k))
  }

  def put(newKey: Int): Unit = _root = put(_root, newKey)

  private[avl] def put(node: Option[Node], newKey: Int): Option[Node] =
    node match {
      case None =>
        Some(Node(newKey))
      case Some(n) =>
        if (n.key > newKey) n.left = put(n.left, newKey)
        else if (n.key < newKey) n.right = put(n.right, newKey)
        Some(adjustSubtree(n, newKey))
    }

  private[avl] def deleteMin(n: Node): Option[Node] = deleteRecursively(Some(n), min(n).key)

  @tailrec
  //private[avl] final def min(n: Node): Node = if (n.left.isEmpty) n else min(n.left.get)
  private[avl] final def min(n: Node): Node = n.left match {
    case None => n
    case Some(left) => min(left)
  }

  private[avl] def deleteRecursively(node: Option[Node], key: Int): Option[Node] = {
    node match {
      case None => None
      case Some(n) =>
        if (key < n.key) {
          n.left = deleteRecursively(n.left, key)
        } else if (key > n.key) {
          n.right = deleteRecursively(n.right, key)
        } else {
          if (n.left.isEmpty)
            return n.right
          else if (n.right.isEmpty)
            return n.left
          else {//both left and right exist. need to replace deleting node with min of right child node
            return Some(doBalance(replaceWithMinOfRightChild(n)))
          }
        }
        Some(doBalance(n))
    }
  }

  /*
    Input must have right child.
    Method finds min of right child, deletes this node, and creates new node with min key.
   */
  private[avl] def replaceWithMinOfRightChild(n: Node) : Node = {
    val minRight = min(n.right.get)
    val updated = Node(minRight.key)
    updated.right = deleteRecursively(n.right, minRight.key)
    updated.left = n.left
    updated
  }

  def delete(key: Int): Unit = _root = deleteRecursively(_root, key)

  private[avl] def adjustSubtree(node: Node, newKey: Int): Node =
    if (node.key == newKey) node //or process node if duplicates are supported
    else doBalance(node)

  def preOrder(): Unit = preOrder(_root)

  private[avl] def preOrder(maybeNode: Option[Node]): Unit = {
    if (maybeNode.isDefined) {
      println(s"${maybeNode.get} ")
      preOrder(maybeNode.get.left)
      preOrder(maybeNode.get.right)
    }
  }

  def isBalanced(): Boolean = isBalanced(_root)

}
