package cs.tree.avl

import org.mockito.IdiomaticMockito
import org.scalatest.{BeforeAndAfterEach, FunSuite, GivenWhenThen}

class NodeTest extends FunSuite
  with GivenWhenThen
  with BeforeAndAfterEach
  with TreeTester
  with IdiomaticMockito{

  test("node allow to get key") {
    assert(Node(10).key == 10)
  }

  test("created node has height = 1 by default") {
    assert(Node(10).height == 1)
  }

  test("created node has no left child") {
    assert(Node(10).left == None)
  }

  test("created node has no right child") {
    assert(Node(10).right == None)
  }

  test("node allows to change its left child") {
    val node = Node(10)
    node.left = Some(Node(1))
    assert(node.left == Some(Node(1)))
  }

  test("node allows to change its right child") {
    val node = Node(10)
    node.right = Some(Node(1))
    assert(node.right == Some(Node(1)))
  }

}
