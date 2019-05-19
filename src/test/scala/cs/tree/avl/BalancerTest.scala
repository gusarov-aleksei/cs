package cs.tree.avl

import org.scalatest.{BeforeAndAfterEach, FunSuite, GivenWhenThen}

/*
  Balancer trait tests
 */
class BalancerTest extends FunSuite
  with GivenWhenThen
  with BeforeAndAfterEach
  with TreeTester {

  var tree: AVLTree = _

  override def beforeEach(): Unit = {
    tree = new AVLTree
    super.beforeEach()
  }

  test("Tree accesses to node.height") {
    val n1 = new Node(1)
    assert(tree.height(Some(n1)) == 1)
    assert(tree.height(None) == 0)
  }

  test("Total height of node without left and right children is 1") {
    Given("Node without children")
    val n1 = new Node(1)

    Then("Its total height is 1")
    assert(tree.totalHeight(n1) == 1)
  }

  test("Total height of node with left or right is (1 + max(left, right))") {
    Given("node with two children")
    val n1 = new Node(2)
    val n2 = new Node(1)
    val n3 = new Node(3)
    n1.right = Some(n3)
    n1.left = Some(n2)

    When("height of one child is 2, another child is 3 ")
    n3.height = 3
    n1.height = 2

    Then("Total height is 4")
    assert(tree.totalHeight(n1) == 4)
  }

  test("Balance factor of node is difference between child left and child right height") {
    Given("node with two children")
    val parent = new Node(2)
    val left = new Node(1)
    val right = new Node(3)
    parent.right = Some(right)
    parent.left = Some(left)

    When("height of left node is 2, right node is 4")
    right.height = 4
    left.height = 2

    Then("Balance factor is -2")
    assert(tree.balanceFactor(parent) == -2)

    When("height of left node is 4, right node is 2")
    right.height = 2
    left.height = 4

    Then("Balance factor is 2")
    assert(tree.balanceFactor(parent) == 2)

    When("height of left node is 3, right node is 3")
    right.height = 3
    left.height = 3

    Then("Balance factor is 2")
    assert(tree.balanceFactor(parent) == 0)

  }

  test("Balance factor None is 0") {
    assert(tree.balanceFactor(None) == 0)
  }


  /*
         10      5
        /       / \
       5   ->  1  10
      /
     1

   */
  test("Tree can rotate branch to the right") {
    Given("Node with left first child and left second child of the first")
    val node = Node(10)
    val leftChild1 = Node(5)
    val leftChild2 = Node(1)
    node.left = Some(leftChild1)
    leftChild1.left = Some(leftChild2)

    Then("Node can be rotated to the left to achieve balance")
    val newParent = tree.rotateRight(node)
    assert(node.left === None,"Node became not parent after rotation")
    assert(newParent.key == 5, "New parent has key = 2")
    assert(newParent.left.get.key == 1)
    assert(newParent.right.get.key == 10)
  }

  /*
         10      5
        /       / \
       5   ->  1  10
      / \         /
     1  8        8
  */
  test("Tree can rotate branch to the right with right child node under rotating node") {
    Given("Node with left first child and left and right second children of the first")
    val node = Node(10)
    val leftChild1 = Node(5)
    val rightChild2 = Node(8)
    val leftChild2 = Node(1)
    node.left = Some(leftChild1)
    leftChild1.left = Some(leftChild2)
    leftChild1.right = Some(rightChild2)

    Then("Branch can be rotated to the right and right child of rotated node moves to another parent")
    val newParent = tree.rotateRight(node)
    assertNode(node.left,(8,1))
    assertNode(Some(newParent), (5,3))
    assertNode(newParent.left, (1,1))
    assertNode(newParent.right, (10,2))
  }


  /*
      3           5
       \         / \
        5   ->  3   6
         \
          6
   */
  test("Tree can rotate branch to the left") {
    Given("Node with right first child and right second child of the first")
    val node = Node(3)
    val rightChild1 = Node(5)
    val rightChild2 = Node(6)
    node.right = Some(rightChild1)
    rightChild1.right = Some(rightChild2)

    Then("Node can be rotated to the left to achieve balance")
    val newParent = tree.rotateLeft(node)
    assert(newParent.key == 5)
    assert(newParent.left.get.key == 3)
    assert(newParent.right.get.key == 6)
    assert(node.right == None, "Node became not a parent after rotation")
  }

  /*

     3           5
      \         / \
       5   ->  3  6
      / \      \
     4   6      4

 */
  test("Tree can rotate branch to the left with left child node under rotating node") {
    Given("node with right first child and right and left second children of the first ")
    val node = Node(3)
    val rightChild1 = Node(5)
    val rightChild2 = Node(6)
    val leftChild2 = Node(4)
    node.right = Some(rightChild1)
    rightChild1.right = Some(rightChild2)
    rightChild1.left = Some(leftChild2)

    Then("node can be rotated to the left and left child of rotated node moves to another parent as right child")
    val newParent = tree.rotateLeft(node)
    assertNode(Some(node), (3,2))
    assertNode(Some(newParent), (5,3))
    assertNode(newParent.left, (3,2))
    assertNode(newParent.right, (6,1))
  }

  /*
              20               10
             /  \            /   \
           10   30    ->    5     20
          / \              / \   / \
         5  15            1  8  15  30
        / \
       1  8
   */
  test("Tree do balance tree to follow height invariant. Left left case ") {
    Given("subtree with height difference greater than 1")
    val root = Node(20)
    root.height = 4
    root.left = Some(Node(10))
    root.left.get.height=3
    root.right = Some(Node(30))
    root.left.get.left = Some(Node(5))
    root.left.get.left.get.height = 2
    root.left.get.right = Some(Node(15))
    root.left.get.left.get.left = Some(Node(1))
    println(root.left.get.left)
    root.left.get.left.get.right = Some(Node(8))

    Then("data structure re-balance subtree")
    val newRoot = tree.doBalance(root)
    assertNode(Some(newRoot), (10, 3))
    assertNode(newRoot.left, (5, 2))
    assertNode(newRoot.right, (20, 2))
    assertNode(newRoot.left.get.left, (1, 1))
    assertNode(newRoot.left.get.right, (8, 1))
    assertNode(newRoot.right.get.left, (15, 1))
    assertNode(newRoot.right.get.right, (30, 1))
  }

  /*
            20              20               15
           /  \            /  |             /  \
         10   30    ->    15  30   ->    10     20
        / \              / |            / \    / \
       5  15           10  18          5  11 18  30
         / \          / |
       11  18        5  11
   */
  test("Tree do balance tree to follow height invariant. Left right case ") {
    Given("subtree with height difference greater than 1")
    val root = Node(20)
    root.height = 4
    root.left = Some(Node(10))
    root.left.get.height=3
    root.right = Some(Node(30))
    root.left.get.left = Some(Node(5))
    root.left.get.right = Some(Node(15))
    root.left.get.right.get.height = 2
    root.left.get.right.get.left = Some(Node(11))
    root.left.get.right.get.right = Some(Node(18))

    Then("data structure re-balance subtree")
    val newRoot = tree.doBalance(root)
    //tree.preOrder(Some(newRoot))
    assertNode(Some(newRoot), (15, 3))
    assertNode(newRoot.left, (10, 2))
    assertNode(newRoot.right, (20, 2))
    assertNode(newRoot.left.get.left, (5, 1))
    assertNode(newRoot.left.get.right, (11, 1))
    assertNode(newRoot.right.get.left, (18, 1))
    assertNode(newRoot.right.get.right, (30, 1))
  }

  /*
            20                30
           /  \             /   \
         10   30    ->    20     45
             / |          / \   / \
            25  45      10  25 40 48
               / \
              40 48
   */
  test("Tree do balance tree to follow height invariant. Right right case ") {
    Given("subtree with height difference greater than 1")
    val root = Node(20)
    root.height = 4
    root.left = Some(Node(10))
    root.right = Some(Node(30))
    root.right.get.height=3

    root.right.get.left = Some(Node(25))
    root.right.get.right = Some(Node(45))
    root.right.get.right.get.height = 2

    root.right.get.right.get.left = Some(Node(40))
    root.right.get.right.get.right = Some(Node(48))

    Then("data structure re-balance subtree")
    val newRoot = tree.doBalance(root)
    tree.preOrder(Some(newRoot))
    assertNode(Some(newRoot), (30, 3))
    assertNode(newRoot.left, (20, 2))
    assertNode(newRoot.right, (45, 2))
    assertNode(newRoot.left.get.left, (10, 1))
    assertNode(newRoot.left.get.right, (25, 1))
    assertNode(newRoot.right.get.left, (40, 1))
    assertNode(newRoot.right.get.right, (48, 1))
  }

  /*
            20                20               25
           /  \             /  |             /   \
         10   30    ->    10  25   ->      20    30
             / \             / |          / \    / \
           25  35          21  30       10  21 28  35
          / \                 / |
         21 28              28  35
   */
  test("Tree do balance tree to follow height invariant. Right left case ") {
    Given("subtree with height difference greater than 1")
    val root = Node(20)
    root.height = 4
    root.left = Some(Node(10))
    root.right = Some(Node(30))
    root.right.get.height=3
    root.right.get.left = Some(Node(25))
    root.right.get.right = Some(Node(35))
    root.right.get.left.get.height = 2
    root.right.get.left.get.left = Some(Node(21))
    root.right.get.left.get.right = Some(Node(28))

    Then("data structure re-balance subtree")
    val newRoot = tree.doBalance(root)
    tree.preOrder(Some(newRoot))

    assertNode(Some(newRoot), (25, 3))
    assertNode(newRoot.left, (20, 2))
    assertNode(newRoot.right, (30, 2))
    assertNode(newRoot.left.get.left, (10, 1))
    assertNode(newRoot.left.get.right, (21, 1))
    assertNode(newRoot.right.get.left, (28, 1))
    assertNode(newRoot.right.get.right, (35, 1))

  }

  test("tree can be balanced (check avl invariant with isBalanced method)") {
    Given("Balanced tree")
    val root = Node(200, 4)
    root.right = Some(Node(400,3))
    root.left = Some(Node(100,2))
    root.left.get.left = Some(Node(10,1))

    root.right.get.left = Some(Node(300,2))
    root.right.get.right = Some(Node(500,2))
    root.right.get.right.get.left = Some(Node(450,1))
    root.right.get.right.get.right = Some(Node(550,1))

    Then("isBalanced returns true")
    assert(tree.isBalanced(Some(root)))
  }

  test("tree can be unbalanced (check avl invariant with isBalanced method)") {
    Given("Unbalanced tree: height of right child is more than left by 2")
    val root = Node(200, 5)
    root.right = Some(Node(400,4))
    root.left = Some(Node(100,2))
    root.left.get.left = Some(Node(10,1))

    root.right.get.left = Some(Node(300,3))
    root.right.get.right = Some(Node(500,3))
    root.right.get.right.get.left = Some(Node(450,2))
    root.right.get.right.get.right = Some(Node(550,1))

    root.right.get.right.get.right.get.left = Some(Node(520,1))
    root.right.get.right.get.right.get.right = Some(Node(620,1))

    Then("isBalanced returns false")
    assert(!tree.isBalanced(Some(root)))
  }

}
