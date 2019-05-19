package cs.trie.mutable

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/*
  Prefix tree data structure
 */
class Trie  {

  //TODO add max of suggestions
  private val root = TrieNode()

  //add non-null string to data structure
  def append(word: String) : Unit = {
    val node = append(word.toList, root)
    node.word = Some(word)
  }

  @tailrec
  private def append(word: List[Char], node: TrieNode): TrieNode =
    word match {
      case Nil => node
      case head::tail => append(tail, node.append(head))
    }

  //provides with suggestion according to input prefix
  def suggest(prefix: String) : List[String] = findNode(prefix.toList) match {
    case None => List.empty
    case Some(n) =>
      val words = new ListBuffer[String]
      collectRec(n, words)
      words.toList
  }

  //@tailrec
  private def collectRec(node: TrieNode, words: ListBuffer[String]) : Unit = {
    if (node.word.isDefined) {
      words += node.word.get
    }
    for((_,child) <- node.children) {collectRec(child, words)}
  }

  //check if non-null string exists
  def contains(word: String): Boolean = findNode(word.toList) match {
    case None => false
    case Some(n) => n.isWord
  }

  private def findNode(prefix: List[Char]): Option[TrieNode] =
    prefix match {
      case Nil => None
      case head::tail => {
        val next = root.getNode(head)
        if (next.isDefined) findRec(tail, next) else None
      }
    }

  @tailrec
  private def findRec(prefix: List[Char], node: Option[TrieNode]): Option[TrieNode] =
    prefix match {
      case Nil => node
      case head::tail => if (node.isDefined) findRec(tail, node.get.getNode(head)) else None
    }

  //remove word from data structure
  def remove(word: String): Unit =
    if (contains(word)) {
      val nodes = new ListBuffer[TrieNode]
      nodes += root
      collectAffectedNodes(word.tail, root.getNode(word.head), nodes)
      removeNodes(nodes)
    }

  private def removeNodes(nodesToRemove: ListBuffer[TrieNode]): Unit = {
    val nodes = nodesToRemove.reverse.toList
    nodes.head.word = None
    if (nodes.head.children.isEmpty) {
      removeRec(nodes.head, nodes.tail)
    }
  }

  @tailrec
  private def removeRec(current: TrieNode, nodes: List[TrieNode]): Unit =
    nodes match {
      case Nil =>
      case head::tail =>
        if (current.children.isEmpty && current.word == None) {
          head.children.remove(current.letter)
          removeRec(head, tail)
        }
    }

  @tailrec
  private def collectAffectedNodes(word: String, current: Option[TrieNode], nodes: ListBuffer[TrieNode]) : Unit = {
    nodes += current.get //all nodes are present because trie contains the word
    word match {
      case "" =>
      case _ => collectAffectedNodes(word.tail, current.get.getNode(word.head), nodes)
    }
  }

}