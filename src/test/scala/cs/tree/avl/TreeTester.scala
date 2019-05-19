package cs.tree.avl

import org.scalatest.Assertions._

trait TreeTester {

  def assertNone(act: Option[Node]): Unit = {
    assert(act.isEmpty, "Node doesn't exist")
  }

  def assertNode(act: Option[Node], exp: (Int, Int)): Unit = {
    assert(act.isDefined, "Node exists")
    assertNode(act.get, exp)
  }

  def assertNode(act: Node, exp: (Int, Int)): Unit = {
    assert(act.key === exp._1, "Key is matched")
    assert(act.height === exp._2, "Height is matched")
  }

}
