package cs.trie.mutable

import org.scalatest._

trait TrieTester extends PrivateMethodTester with Matchers  {

  def retrieveRoot(trie: Trie) = trie invokePrivate PrivateMethod[TrieNode]('root)()

  def retrieveNode(trie: Trie, prefix: String) : Option[TrieNode] = {
    val findNodeMethod = PrivateMethod[Option[TrieNode]]('findNode)
    trie invokePrivate findNodeMethod(prefix.toList)
  }

  def assertNode(node: Option[TrieNode], expected: (Char, Byte, Option[String])): Unit = {
    assert(node.isDefined, "TrieNode isn't defined for expected._1. Something went wrong.")
    assert(node.get.letter === expected._1)
    assert(node.get.children.size === expected._2)
    assert(node.get.word === expected._3)
  }

}
