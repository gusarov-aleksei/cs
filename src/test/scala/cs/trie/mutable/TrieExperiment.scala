package cs.trie.mutable

import java.util.concurrent.TimeUnit

import org.scalatest.FunSuite

import org.scalatest._
import util.{FileLoader, ResourceMeter}

class TrieExperiment extends FunSuite with Matchers with FileLoader with ResourceMeter {

  test("Compare trie and list resource consumption") {
    val trie = new Trie
    val words = readTextFile("words.txt")

    val result = measureTime(words.foreach(s => trie.append(s)))
    println(s"Trie loading took: ${result._2.toMillis} ms")
    val prefix = "zuc"
    val trieResult = measureTime(trie.suggest(prefix))
    //time complexity equals to log(length of prefix) + length of words found for requested prefix
    println(s"Searching of suggestions in Trie took: ${trieResult._2.toUnit(TimeUnit.MILLISECONDS)} ms")
    val listResult = measureTime(words.filter(s=>s.startsWith(prefix)))
    //time complexity equals to linear amount of words and depends on prefix, in worst case it iterate full list
    //search via trie is faster 8-15 times
    println(s"Searching of suggestions in List took: ${listResult._2.toUnit(TimeUnit.MILLISECONDS)} ms")
    trieResult._1 should contain theSameElementsAs listResult._1
    println(s"Found suggestions: ${trieResult._1}")
    //println(s"Size of Trie: ${size(trie)} bytes")
    //println(s"Size of List: ${size(words)} bytes")
  }

  /*
    Here is example of resource consumption for 370100 words
    Trie loading took: 752 ms, 816 ms, 870 ms
    Searching of suggestions in Trie took: 0.775867 ms, 0.964743 ms, 1.269799 ms
    Searching of suggestions in List took: 14.35223 ms, 12.547425 ms, 11.041198 ms (in case of "zuc" prefix, worst case)
    Size of Trie: 198268888 bytes
    Size of List: 28461544 bytes
    (size calculation took about 2 min)
   */

}
