package cs.tree.avl


import org.scalatest.{BeforeAndAfterEach, FunSuite, GivenWhenThen}
import org.mockito.IdiomaticMockito



class AVLTreeTest extends FunSuite
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

  test("empty tree has no root node") {
    assertNone(tree.root)
  }

  test("tree creates node for every key") {
    When("put key into empty tree")
    tree.put(1)

    Then("tree creates node with defined key")
    val node = tree.root
    assertNode(node, (1, 1))//node with key = 1 and height = 1
    assertNone(node.get.left) //without children
    assertNone(node.get.right)
  }

  //some examples with mocking
  test("tree calls balance operation to adjust node according to height invariant (if node key is not duplicated)") {
    Given("some node with key 20")
    val root = Node(20)

    When("non-existing key is added")
    val mockTree = mock[AVLTree]
    mockTree.adjustSubtree(root, 10) shouldCall realMethod
    mockTree.adjustSubtree(root, 10)

    Then("re-balance is called")
    mockTree.doBalance(root) was called
  }

  test("tree doesn't call balance operation to adjust node according to height invariant (if node key is duplicated)") {
    Given("some node with key 20")
    val root = Node(20)

    When("existing key is added")
    val mockTree = mock[AVLTree]
    mockTree.adjustSubtree(root, 20) shouldCall realMethod
    mockTree.adjustSubtree(root, 20)

    Then("re-balance is not called")
    mockTree.doBalance(root) wasNever called
  }

  test("tree follows invariant: left node key is less than parent key, right node key is greater than parent key") {
    When("tree with 1, 2, 3 keys")
    tree.putAll(1 to 3)

    Then("root node key = 2, left child is 1 and less than 2, right child is 3 and greater than 2")
    val root = tree.root
    assertNode(root, (2, 2))
    assertNode(root.get.left, (1,1))
    assertNode(root.get.right, (3,1))
  }

  test("tree allows to put keys and organizes hierarchy of nodes based on keys") {
    When("to put keys from 1 to 5 into tree")
    tree.putAll(1 to 5)

    Then("tree creates 5 nodes for 5 different keys and organizes them into hierarchy")
    val root = tree.root
    assertNode(root, (2, 3))
    assertNode(root.get.left, (1,1))
    assertNode(root.get.right, (4,2))
    assertNode(root.get.right.get.left, (3,1))
    assertNode(root.get.right.get.right, (5,1))
  }

  test("tree allows to get node by key") {
    When("tree has keys")
    tree.putAll(1 to 100)

    Then("method get returns Some(node) by key")
    //validate state of some pieces of tree
    assertNode(tree.get(64), (64,7))//root of tree, height = 7
    assertNode(tree.get(20), (20,3))
    assertNode(tree.get(1), (1,1)) //the most left always has height = 1
    assertNode(tree.get(100), (100,1)) //the most right always has height = 1
  }

  test("tree with 1 000 000 nodes has height = 20 (just for information)") {
    When("tree has 1000000 keys")
    tree.putAll(1 to 1000000)

    Then("tree height is 1000000")
    //validate state of some pieces of tree
    assertNode(tree.root, (524288, 20))//tree height = 20
    assertNode(tree.get(1), (1, 1))
    assertNode(tree.get(1000000), (1000000, 1))
  }

  test("tree allows to get node by key: negative case") {
    When("tree has keys")
    tree.putAll(1 to 100)

    Then("method get with non-existing keys as input returns None")
    assert(tree.get(200).isEmpty)
  }

  test("tree allows to check if it contains key") {
    When("some keys are added to tree")
    tree.putAll(1 to 100)

    Then("method contains returns true for all existing keys")
    (1 to 100).foreach(key =>  assert(tree.contains(key), "tree contains key added before"))
  }

  test("tree allows to check if it contains key: negative case") {
    When("some keys are added to tree")
    tree.putAll(1 to 100)

    Then("method 'contains' returns false for some non-existing key ")
    (109 to 250).foreach(key =>  assert(!tree.contains(key), "tree doesn't contain key added before"))
  }

  test("tree allows to delete key from its structure") {
    When("Tree has keys")
    tree.putAll(1 to 100)

    When("method delete(50) is called")
    tree.delete(50)

    Then("node with key = 50 is deleted")
    assert(!tree.contains(50), "tree doesn't contain deleted key")
    //verify. other keys still exist in tree
    (1 to 49).foreach(key =>  assert(tree.contains(key), "tree contains non-deleted key"))
    (51 to 100).foreach(key =>  assert(tree.contains(key),"tree contains non-deleted key"))
  }

  /*
        5
       / \
      4   10
          /
         8
   */
  test("tree does re-balancing (changes parent-child hierarchy and recalculates height of nodes) when child is being added") {
    Given("empty tree")

    When("to add key = 10")
    tree.put(10)

    Then("tree creates node with key = 10 and height = 1(default height)")
    assertNode(tree.root, (10,1))

    When("to add key = 5")
    tree.put(5)

    Then("tree creates node with key = 5, adds it under node 10 and changes its height to 2")
    assertNode(tree.root, (10,2))
    assertNode(tree.root.get.left, (5,1))

    When("to add key = 4")
    tree.put(4)

    Then("tree creates node with key = 4, adds it under node 5 and does re-balancing (left rotation)")
    assertNode(tree.root, (5,2))
    assertNode(tree.root.get.left, (4,1))
    assertNode(tree.root.get.right, (10,1))

    When("to add key = 8")
    tree.put(8)

    Then("tree creates node with key = 8, adds it under node 10 and recalculates all its parents height")
    assertNode(tree.root, (5,3))
    assertNode(tree.root.get.left, (4,1))
    assertNode(tree.root.get.right, (10,2))
    assertNode(tree.root.get.right.get.left, (8,1))
  }

  test("tree recalculates height of parent nodes when child is being removed") {
    Given("tree with some nodes")
    tree.putAll(Array(10,5,4,8))

    When("key is being removed from tree")
    tree.delete(8)

    Then("data structure does re-balance of subtree and recalculates height of parent ")
    assert(!tree.contains(8), "node with key = 8 doesn't exist")
    assertNode(tree.root, (5,2))
    assertNode(tree.root.get.left, (4,1))
    assertNode(tree.root.get.right, (10,1))
  }

  test("tree has ability to delete min node") {
    Given("tree with some nodes")
    tree.putAll(Array(100, 50, 40, 200, 70, 35, 190, 45, 140, 110, 20, 75, 190))

    When("min key is being removed from tree")
    val updated = tree.deleteMin(tree.root.get)

    Then("data structure doesn't contain min")
    Array(100, 50, 40, 200, 70, 35, 190, 45, 140, 110, 75, 190).foreach(
      key => assert(tree.contains(updated, key), "tree doesn't contain key = " + key)
    )
    assert(!tree.contains(20), "tree contains deleted key = 20")
  }
  /*
      replace 50 with 70: 50 -> right 140 -> left 100 -> left 70
   */
  test("tree has ability to replace given node with minimum of its right child") {
    Given("tree with some nodes")
    tree.putAll(Array(100, 50, 40, 200, 70, 35, 190, 45, 140, 110, 20, 75, 190))
    //root key is 50 for present keys

    When("input node key = 50 is replaced with key of min node of right child")
    val updated = tree.replaceWithMinOfRightChild(tree.root.get)

    Then("data structure doesn't contain 50")
    Array(100, 40, 200, 70, 35, 190, 45, 140, 110, 75, 190, 20).foreach(
      key => assert(tree.contains(Some(updated), key), "tree doesn't contain key = " + key)
    )
    assert(!tree.contains(Some(updated), 50), "tree contains deleted key = 50")
    assert(updated.key == 70)
  }

  /*
      remove 50

                  100                                            100
                /     \                                        /    \
              50      another part of subtree                 54     another part of subtree
            /   \          ...                              /   \            ...
          40    60                                         40    60
         / |    / \                               ->      / |    / \
       30  45  55 70                                     30 45  55 70
              / \                                               \
            54  56                                              56
   */
  test("tree provides with ability of removing its nodes") {
    Given("tree with some nodes")
    tree.putAll(Array(100, 50, 150, 40, 60, 120, 200, 30, 45, 55, 70, 130, 140, 190, 220, 54, 56))

    When("key = 50 is deleted")
    tree.delete(50)

    Then("data structure doesn't contain 50, key = 50 is replaced by 54")
    assert(!tree.contains(50), "tree contains deleted key = 50")
    Array(100, 100, 150, 40, 60, 120, 200, 30, 45, 55, 70, 130, 140, 190, 220, 54, 56).foreach(
      key => assert(tree.contains(key), "tree doesn't contain key = " + key)
    )
    assertNode(tree.get(54), (54, 4))//before replacing its height was 1
    assertNode(tree.get(55), (55, 2))
    assertNode(tree.get(55).get.right, (56, 1))
  }

  /*
      remove 50

                 100                                          100
                /    \                                       /    \
              50     another part of subtree               55     another part of subtree
            /   \          ...                            /  \           ...
          40    60                                ->     40   60
         / |    / \                                     / |   / \
       30  45  55 70                                   30 45 56 70
                \
                56
   */
  test("tree provides with ability of removing its nodes (another case without key = 54)") {
    Given("tree with some nodes")
    tree.putAll(Array(100, 50, 150, 40, 60, 120, 200, 30, 45, 55, 70, 130, 140, 190, 220, 56))

    When("input node key = 50 is replaced with key of min node of right child")
    tree.delete(50)

    Then("data structure doesn't contain 50")
    assert(!tree.contains(50), "tree contains deleted key = 50")
    Array(100, 100, 150, 40, 60, 120, 200, 30, 45, 55, 70, 130, 140, 190, 220, 56).foreach(
      key => assert(tree.contains(key), "tree doesn't contain key = " + key)
    )
    assertNode(tree.get(55), (55, 3))//before replacing its height was 2
    assertNode(tree.get(55).get.right, (60,2))
    assertNode(tree.get(60).get.left, (56, 1))
  }

}
