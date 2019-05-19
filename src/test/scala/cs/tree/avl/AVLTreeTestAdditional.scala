package cs.tree.avl

import org.mockito.IdiomaticMockito
import org.scalatest.{BeforeAndAfterEach, FunSuite, GivenWhenThen}

/*
  Additional test with manually created tree nodes
 */
class AVLTreeTestAdditional extends FunSuite
  with GivenWhenThen
  with BeforeAndAfterEach
  with TreeTester
  with IdiomaticMockito{

  var tree: AVLTree = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    tree = new AVLTree
  }

  override def afterEach(): Unit = {
    super.afterEach()
    assert(tree.isBalanced(), "tree follows avl invariant: is balanced")
  }

  test("tree does re-balancing when child is being deleted in case of unbalanced structure(test with manually created nodes)") {
    Given("some tree structure created manually (it is already unbalanced: right subtree is longer than left)")
    val root = Node(10 ,5)
    root.left = Some(Node(5, 2))
    root.left.get.left = Some(Node(4))
    root.left.get.right = Some(Node(8))

    root.right = Some(Node(20,4))
    root.right.get.left = Some(Node(15,3))

    root.right.get.left.get.left = Some(Node(12,2))
    root.right.get.left.get.left.get.left = Some(Node(11))
    root.right.get.left.get.left.get.right = Some(Node(14))

    root.right.get.left.get.right = Some(Node(17,2))
    root.right.get.left.get.right.get.left = Some(Node(16))
    root.right.get.left.get.right.get.right = Some(Node(19))

    root.right.get.right = Some(Node(30,3))
    root.right.get.right.get.left = Some(Node(25))
    root.right.get.right.get.right = Some(Node(39,2))

    root.right.get.right.get.right.get.left = Some(Node(35))
    root.right.get.right.get.right.get.right = Some(Node(50))

    When("key = 15 is being deleted from tree structure")
    val newRoot = tree.deleteRecursively(Some(root), 15)

    Then("data structure doesn't contain deleted key = 15 and does re-balance subtree: key = 20 becomes root")

    assertNode(newRoot, (20,5))
    assertNode(newRoot.get.left, (10,4))
    assertNode(newRoot.get.left.get.left, (5,2))
    assertNode(newRoot.get.left.get.left.get.left, (4,1))
    assertNode(newRoot.get.left.get.left.get.right, (8,1))

    assertNone(newRoot.get.left.get.left.get.left.get.left)
    assertNone(newRoot.get.left.get.left.get.left.get.right)
    assertNone(newRoot.get.left.get.left.get.right.get.left)
    assertNone(newRoot.get.left.get.left.get.right.get.right)

    assertNode(newRoot.get.left.get.right, (16,3))
    assertNode(newRoot.get.left.get.right.get.left, (12,2))
    assertNode(newRoot.get.left.get.right.get.left.get.left, (11,1))
    assertNone(newRoot.get.left.get.right.get.left.get.left.get.left)
    assertNone(newRoot.get.left.get.right.get.left.get.left.get.right)

    assertNode(newRoot.get.left.get.right.get.left.get.right, (14,1))

    assertNode(newRoot.get.left.get.right.get.right, (17,2))
    assertNode(newRoot.get.left.get.right.get.right.get.right, (19,1))
    assertNone(newRoot.get.left.get.right.get.right.get.left)

    assertNode(newRoot.get.right, (30,3))
    assertNode(newRoot.get.right.get.left, (25,1))
    assertNone(newRoot.get.right.get.left.get.left)
    assertNone(newRoot.get.right.get.left.get.right)
    assertNode(newRoot.get.right.get.right, (39,2))
    assertNode(newRoot.get.right.get.right.get.left, (35,1))
    assertNone(newRoot.get.right.get.right.get.left.get.left)
    assertNone(newRoot.get.right.get.right.get.left.get.right)

    assertNode(newRoot.get.right.get.right.get.right, (50,1))
    assertNone(newRoot.get.right.get.right.get.right.get.right)
    assertNone(newRoot.get.right.get.right.get.right.get.left)

    assert(!tree.contains(newRoot, 15), "key = 15 is expected to be deleted")//no key = 15 in tree
  }

  test("tree can get node by key from subtree via method get(node: Option[Node], key: Int) ") {
    Given("hierarchy of nodes")
    val root = Some(Node(15,3))
    root.get.left = Some(Node(12,2))
    root.get.left.get.left = Some(Node(11))
    root.get.left.get.right = Some(Node(14))
    root.get.right = Some(Node(17,2))
    root.get.right.get.left = Some(Node(16))
    root.get.right.get.right = Some(Node(19))

    Then("tree can retrieve all its nodes by key")
    assertNode(tree.get(root, 15), (15, 3))
    assertNode(tree.get(root, 12), (12, 2))
    assertNode(tree.get(root, 11), (11, 1))
    assertNode(tree.get(root, 14), (14, 1))
    assertNode(tree.get(root, 17), (17, 2))
    assertNode(tree.get(root, 16), (16, 1))
    assertNode(tree.get(root, 19), (19, 1))
  }

  test("tree allows to delete node: right node without children") {
    Given("hierarchy of nodes")
    val root = Some(Node(15,3))
    root.get.left = Some(Node(12,2))
    root.get.left.get.left = Some(Node(11))
    root.get.left.get.right = Some(Node(14))
    root.get.right = Some(Node(17,2))
    root.get.right.get.left = Some(Node(16))
    root.get.right.get.right = Some(Node(19))

    When("right node without children is being deleted from tree (node with key = 19 is being deleted)")
    val newRoot = tree.deleteRecursively(root, 19)

    Then("data structure doesn't contain deleted node")
    assert(tree.get(newRoot, 19) == None, "Node with 19 key is expected to be deleted")
    assert(newRoot.get.right.get.right == None)
    assertNode(newRoot.get.right, (17, 2))
    assertNode(newRoot.get.right.get.left, (16, 1))
  }

  test("tree allows to delete node: left node without children") {
    Given("hierarchy of nodes")
    val root = Some(Node(15,3))
    root.get.left = Some(Node(12,2))
    root.get.left.get.left = Some(Node(11))
    root.get.left.get.right = Some(Node(14))
    root.get.right = Some(Node(17,2))
    root.get.right.get.left = Some(Node(16))
    root.get.right.get.right = Some(Node(19))

    When("left node without children is being deleted from tree")
    val newRoot = tree.deleteRecursively(root, 16)

    Then("data structure doesn't contain deleted node")
    assert(tree.get(newRoot, 16) == None, "Node with 16 key is expected to be deleted")
    assert(newRoot.get.right.get.left == None)
    assertNode(newRoot.get.right, (17, 2))
    assertNode(newRoot.get.right.get.right, (19, 1))
  }

  test("tree allows to delete node: right node with one left child") {
    Given("tree with some nodes")
    val root = Some(Node(15,3))
    root.get.left = Some(Node(12,2))
    root.get.left.get.left = Some(Node(11))
    root.get.left.get.right = Some(Node(14))
    root.get.right = Some(Node(17,2))
    root.get.right.get.left = Some(Node(16))

    When("right node with one child is being deleted from tree")
    val newRoot = tree.deleteRecursively(root, 17)

    Then("deleted node is replaced by its left child")
    assertNode(newRoot, (15, 3))
    assertNode(newRoot.get.right, (16, 1))
    assert(newRoot.get.right.get.right == None)
    assert(newRoot.get.right.get.left == None)
    assert(tree.get(newRoot, 17) == None, "Node with 17 key is expected to be deleted")
  }

  test("tree allows to delete node: right node with one right child") {
    Given("tree with some nodes")
    val root = Some(Node(15,3))
    root.get.left = Some(Node(12,2))
    root.get.left.get.left = Some(Node(11))
    root.get.left.get.right = Some(Node(14))
    root.get.right = Some(Node(17,2))
    root.get.right.get.right = Some(Node(19))

    When("right node with one right child is being deleted from tree")
    val newRoot = tree.deleteRecursively(root, 17)

    Then("deleted node is replaced by its right child")
    assertNode(newRoot, (15, 3))
    assertNode(newRoot.get.right, (19, 1))
    assert(newRoot.get.right.get.right == None)
    assert(newRoot.get.right.get.left == None)
    assert(tree.get(newRoot, 17) == None, "Node with 17 key is expected to be deleted")
  }

  test("tree allows to delete node: left node with the one left child") {
    Given("tree with some nodes")
    val root = Some(Node(15,3))
    root.get.left = Some(Node(12,2))
    root.get.left.get.left = Some(Node(11))
    root.get.right = Some(Node(17,2))
    root.get.right.get.right = Some(Node(19))
    root.get.right.get.left = Some(Node(16))

    When("left node with one left child is being deleted from tree")
    val newRoot = tree.deleteRecursively(root, 12)

    Then("deleted node is replaced by its left child")
    assertNode(newRoot, (15, 3))
    assertNode(newRoot.get.left, (11, 1))
    assert(newRoot.get.left.get.right == None)
    assert(newRoot.get.left.get.left == None)
    assert(tree.get(newRoot, 12) == None, "Node with 12 key is expected to be deleted")
  }

  test("tree allows to delete node: left node with the one right child") {
    Given("tree with some nodes")
    val root = Some(Node(15,3))
    root.get.left = Some(Node(12,2))
    root.get.left.get.right = Some(Node(14))
    root.get.right = Some(Node(17,2))
    root.get.right.get.right = Some(Node(19))
    root.get.right.get.left = Some(Node(16))

    When("left node with the one right child is being deleted from tree")
    val newRoot = tree.deleteRecursively(root, 12)

    Then("deleted node is replaced by its right child")
    assertNode(newRoot, (15, 3))
    assertNode(newRoot.get.left, (14, 1))
    assertNone(newRoot.get.left.get.right)
    assertNone(newRoot.get.left.get.left)
    assert(tree.get(newRoot, 12) == None, "Node with 12 key is expected to be deleted")
  }

  test("tree allows to delete node: one child node is deleted and its parent node height is being changed") {
    Given("tree with some nodes")
    val root = Some(Node(15,3))
    root.get.left = Some(Node(12,2))
    root.get.left.get.right = Some(Node(14))
    root.get.right = Some(Node(17))

    When("left node with the one right child is being deleted from tree")
    val newRoot = tree.deleteRecursively(root, 12)

    Then("parent node height is changed from 3 to 2")
    assertNode(newRoot, (15, 2))
  }

  test("tree allows to delete node: node with two child is being deleted") {
    Given("tree with some nodes")
    val root = Some(Node(50,4))
    root.get.left = Some(Node(25,2))
    root.get.left.get.left = Some(Node(15))
    root.get.left.get.right = Some(Node(30))
    root.get.right = Some(Node(100,3))
    root.get.right.get.left = Some(Node(75,1))
    root.get.right.get.right = Some(Node(125,2))
    root.get.right.get.right.get.left = Some(Node(115))
    root.get.right.get.right.get.right = Some(Node(135))

    When("right node with two child is being deleted")
    val newRoot = tree.deleteRecursively(root,100)

    Then("deleted node is replaced with min node of its right child (100 -> 115 )")
    assertNode(newRoot, (50, 4))
    assertNode(newRoot.get.right, (115, 3))
    assertNode(newRoot.get.right.get.right, (125, 2))
    assertNode(newRoot.get.right.get.left, (75, 1))
    assertNone(newRoot.get.right.get.right.get.left)
  }

  test("tree allows to find its min") {
    Given("tree with some nodes")
    val root = Some(Node(50,3))
    root.get.left = Some(Node(25,2))
    root.get.left.get.left = Some(Node(15))
    root.get.left.get.right = Some(Node(30))
    root.get.right = Some(Node(100,2))
    root.get.right.get.left = Some(Node(75))
    root.get.right.get.right = Some(Node(125))

    Then("min method returns node with min key of subtree (the most left node of subtree)")
    assertNode(tree.min(root.get), (15,1))
  }

  test("in case of only one node, method min returns this one node") {
    assertNode(tree.min(Node(10)), (10,1))
  }

  test("tree allows to delete min for node") {
    Given("tree with some nodes")
    val root = Some(Node(50,3))
    root.get.left = Some(Node(25,2))
    root.get.left.get.left = Some(Node(15))
    root.get.left.get.right = Some(Node(30))
    root.get.right = Some(Node(100,2))
    root.get.right.get.left = Some(Node(75))
    root.get.right.get.right = Some(Node(125))

    When("delete min is called for node")
    val newRoot = tree.deleteMin(root.get)

    Then("node with min key = 15 is being deleted")
    assertNone(tree.get(newRoot, 15))
    assertNone(newRoot.get.left.get.left)
    assertNode(newRoot,(50,3))
    assertNode(newRoot.get.left,(25,2))
    assertNode(newRoot.get.left.get.right, (30,1))
    assertNode(newRoot.get.right,(100,2))
    assertNode(newRoot.get.right.get.left, (75,1))
    assertNode(newRoot.get.right.get.right, (125,1))
  }

}
