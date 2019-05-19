
package cs.trie.mutable

import scala.collection.mutable.HashMap

private[mutable] class TrieNode(val letter: Char) {

  def this() {
    this(TrieNode.EmptyChar)
  }

  //TODO add rank of word
  var word : Option[String] = None
  //TODO possible to replace with array of 26 TrieNodes(if english ABC)
  private val _children = new HashMap[Char, TrieNode]

  def isWord : Boolean = word.isDefined

  private[mutable] def getNode(c: Char): Option[TrieNode] = children.get(c)

  private[mutable] def children = _children

  private[mutable] def append(c: Char): TrieNode = children.getOrElseUpdate(c, TrieNode(c))

  override def toString: String = {
    "letter=%c word=%s children=%d".format(letter, word, children.size)
  }
}

private[mutable] object TrieNode{
  val EmptyChar = Char.MinValue
  def apply(letter: Char): TrieNode = new TrieNode(letter)
  def apply(): TrieNode = new TrieNode()
}
