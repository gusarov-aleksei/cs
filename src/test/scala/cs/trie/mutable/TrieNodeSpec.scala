package cs.trie.mutable

import org.scalatest._

class TrieNodeSpec extends FlatSpec with Matchers {

  "TrieNode" should "be created with letter" in {
    val node = TrieNode('a')
    node.letter should be ('a')
  }

  it should "be created with no children" in {
    val node = TrieNode('a')
    node.children should have size 0
  }

  it should "be created as non-word by default" in {
    val node = TrieNode('a')
    node.word should be (None)
    node.isWord should be (false)
  }

  it should "be able to be a word" in {
    val node = TrieNode('a')
    node.word = Some("a")
    node.isWord should be (true)
  }

  it should "append child node when appending character" in {
    val node = TrieNode('a')
    val b = node.append('b')
    node.children.size should be (1)
    node.children should contain('b' -> b)
  }

  it should "create child node with appropriate character when appending character" in {
    val node = TrieNode('b')
    val child = node.append('b')
    child.letter should be ('b')
  }

  it should "allow to child retrieve node by character" in {
    val node = TrieNode('b')
    val child = node.append('b')
    node.getNode('b') should be (Some(child))
  }

}
