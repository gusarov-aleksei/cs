package cs.trie.mutable

import org.scalatest._

class TrieSpec extends FlatSpec
  with GivenWhenThen with Matchers with TrieTester with BeforeAndAfterEach {

  var trie: Trie = _

  override def beforeEach(): Unit = {
    trie = new Trie
    super.beforeEach()
  }

  /*override def afterEach(): Unit ={
    try {
      super.afterEach()
    }
    finally {
      //clean up something
    }
  }*/

  "Empty Trie" should "have empty root TrieNode" in {

    Then("root TrieNode has empty char, empty children and it is not a word")
    assertNode(Some(retrieveRoot(trie)), (TrieNode.EmptyChar, 0, None))
  }

  it should "suggest nothing for any prefix" in {

    Then("no suggestion is returned for any word")
    assert(trie.suggest("") === List.empty)
    assert(trie.suggest("any") === List.empty)
  }

  it should "not contain any prefix" in {

    Then("it doesn't contain any word")
    assert(trie.contains("") === false)
    assert(trie.contains("any") === false)
  }

  it should "not change state if empty string or any words are removed" in {

    When("remove empty word is called")
    trie.remove("")
    trie.remove("any")

    Then("Its root node has empty char, empty children and is not a word")
    assertNode(Some(retrieveRoot(trie)), (TrieNode.EmptyChar, 0, None))
  }

  "Trie" should "place word into branch of TrieNodes" in {

    When("word 'hello' is added")
    trie.append("hello")

    Then("all letters are placed into one branch of TrieNodes: h->e->l->l->o")
    val root = retrieveRoot(trie)
    assertNode(Some(root), (TrieNode.EmptyChar, 1, None))
    val h = root.getNode('h')
    assertNode(h, ('h', 1, None))
    val e = h.get.getNode('e')
    assertNode(e, ('e', 1, None))
    val l1 = e.get.getNode('l')
    assertNode(l1, ('l', 1, None))
    val l2 = l1.get.getNode('l')
    assertNode(l2, ('l', 1, None))
    val o = l2.get.getNode('o')
    assertNode(o, ('o', 0, Some("hello")))
  }

  it should "place words with similar prefix to the same branch of TrieNodes" in {

    When("words 'hello', 'heat' are added")
    trie.append("hello")
    trie.append("heat")

    Then("letters of similar prefix are placed into one branch of TrieNodes h->e, other letters are in different branches")
    val root = retrieveRoot(trie)
    assertNode(Some(root), (TrieNode.EmptyChar, 1, None))
    val h = root.getNode('h')
    assertNode(h, ('h', 1, None))
    val e = h.get.getNode('e')
    assertNode(e, ('e', 2, None))//it has two children
    val l1 = e.get.getNode('l')
    assertNode(l1, ('l', 1, None))
    val l2 = l1.get.getNode('l')
    assertNode(l2, ('l', 1, None))
    val o = l2.get.getNode('o')
    assertNode(o, ('o', 0, Some("hello")))

    val a = e.get.getNode('a')
    assertNode(a, ('a', 1, None))
    val t = a.get.getNode('t')
    assertNode(t, ('t', 0, Some("heat")))
  }

  it should "place words with non-similar prefix to different branches of root" in {

    When("words 'a', 'trie' are added")
    trie.append("a")
    trie.append("trie")

    Then("letters are placed into two different branches")
    val root = retrieveRoot(trie)
    assertNode(Some(root), (TrieNode.EmptyChar, 2, None))
    val a = root.getNode('a')
    assertNode(a, ('a', 0, Some("a")))
    val t = root.getNode('t')
    assertNode(t, ('t', 1, None))
    val r = t.get.getNode('r')
    assertNode(r, ('r', 1, None))
    val i = r.get.getNode('i')
    assertNode(i, ('i', 1, None))
    val e = i.get.getNode('e')
    assertNode(e, ('e', 0, Some("trie")))
  }

  it should "extend branch which is a prefix for another word" in {

    Given("Trie with added word 'he'")
    trie.append("he")

    When("word 'hello' with the same prefix is added")
    trie.append("hello")

    Then("branch 'h->e' is extended with 'l->l->o' nodes")
    val root = retrieveRoot(trie)
    assertNode(Some(root), (TrieNode.EmptyChar, 1, None))
    val h = root.getNode('h')
    assertNode(h, ('h', 1, None))
    val e = h.get.getNode('e')
    assertNode(e, ('e', 1, Some("he")))//'he' is word
    val l1 = e.get.getNode('l')
    assertNode(l1, ('l', 1, None))
    val l2 = l1.get.getNode('l')
    assertNode(l2, ('l', 1, None))
    val o = l2.get.getNode('o')
    assertNode(o, ('o', 0, Some("hello")))
  }

  it should "mark node as Some(word) at already existing branch when shorter word with the same prefix is appended" in {

    Given("trie with added word 'hello'")
    trie.append("hello")

    When("shorter word 'he' is added")
    trie.append("he")

    Then("trie marks node 'e' with Some(he) sign")
    val root = retrieveRoot(trie)
    assertNode(Some(root), (TrieNode.EmptyChar, 1, None))
    val h = root.getNode('h')
    assertNode(h, ('h', 1, None))
    val e = h.get.getNode('e')
    assertNode(e, ('e', 1, Some("he")))//'he' is word
    val l1 = e.get.getNode('l')
    assertNode(l1, ('l', 1, None))
    val l2 = l1.get.getNode('l')
    assertNode(l2, ('l', 1, None))
    val o = l2.get.getNode('o')
    assertNode(o, ('o', 0, Some("hello")))
  }

  it should "not hold duplicate words" in {

    Given("Trie with added word 'hello'")
    trie.append("hello")

    When("the same word 'hello' is added")
    trie.append("hello")

    Then("there is only one branch of nodes h->e->l->l->o")
    val root = retrieveRoot(trie)
    assertNode(Some(root), (TrieNode.EmptyChar, 1, None))
    val h = root.getNode('h')
    assertNode(h, ('h', 1, None))
    val e = h.get.getNode('e')
    assertNode(e, ('e', 1, None))
    val l1 = e.get.getNode('l')
    assertNode(l1, ('l', 1, None))
    val l2 = l1.get.getNode('l')
    assertNode(l2, ('l', 1, None))
    val o = l2.get.getNode('o')
    assertNode(o, ('o', 0, Some("hello")))
  }

  it should "find existing node by prefix" in {

    Given("Trie with added one word 'hello'")
    trie.append("hello")

    When("trying to find with existing in Trie prefix 'he'")
    val e = retrieveNode(trie, "he")

    Then("node is present")
    assertNode(e, ('e', 1, None))
  }

  it should "not find non-existing node by prefix" in {

    Given("trie with some words")
    trie.append("hello")
    trie.append("heap")
    trie.append("health")

    When("find trie node by non-existing prefix")
    val node = retrieveNode(trie, "hi")

    Then("node is not found")
    assert(node === None)
  }

  it should "provide user with one suggestion of existing word by matched prefix" in {

    When("trie with one added word")
    trie.append("hello")

    Then("suggestion with prefix 'he' is retrieved")
    trie.suggest("he") should contain only ("hello")
  }

  it should "provide user with several suggestions of existing words by matched prefix" in {

    Given("trie with some added words")
    trie.append("hello")
    trie.append("heap")
    trie.append("hope")
    trie.append("health")
    trie.append("here")
    trie.append("hat")

    When("call for retrieving suggestions by prefix 'he'")
    val suggestions = trie.suggest("he")

    Then("suggestions with prefix 'he' are retrieved")
    suggestions should contain only ("hello", "heap", "health", "here")
  }

  it should "not contain word if word wasn't appended before" in {

    trie.append("first")
    trie.append("second")
    trie.append("fine")
    assert(!trie.contains("hello"))
  }

  it should "contain word if word was added before" in {

    When("some words are appended to trie")
    trie.append("first")
    trie.append("second")
    trie.append("fine")

    Then("trie contains appended words")
    assert(trie.contains("first"))
    assert(trie.contains("second"))
    assert(trie.contains("fine"))
  }

  it should "remove existing trie nodes from root node if it is necessary" in {

    Given("trie with added word")
    trie.append("hello")

    When("word is removed from trie structure")
    trie.remove("hello")

    Then("trie nodes h->e->l->l->o are removed from root node")
    assertNode(Some(retrieveRoot(trie)), (TrieNode.EmptyChar, 0, None))
    assert(!trie.contains("hello"))
  }

  it should "remove existing trie nodes from root node and doesn't affect common prefix" in {

    Given("trie with added words with common prefix 'he'")
    trie.append("hello")
    trie.append("he")

    When("word is removed from trie structure")
    trie.remove("hello")

    Then("trie nodes h->e->l->l->o are removed from root node")
    assertNode(Some(retrieveRoot(trie)), (TrieNode.EmptyChar, 1, None))
    assert(!trie.contains("hello"))
    assert(trie.contains("he"))
  }

  it should "don't change state if non-existing word with existing prefix is removed" in {

    Given("Trie with added word")
    trie.append("hello")

    When("word is removed from Trie structure")
    trie.remove("he")

    Then("Trie still contains word")
    assert(trie.contains("hello"))//nothing happened
  }

  it should "don't change state if empty string is removed (Trie with some word)" in {

    Given("Trie with added word")
    trie.append("hello")

    When("removing empty string is called")
    trie.remove("")

    Then("Trie still contains word")
    assert(trie.contains("hello"))
  }

  it should "remove existing word from its structure and doesn't affect another words with common prefix" in {

    Given("Trie with added 'hello', 'health' words")
    trie.append("hello")
    trie.append("health")

    When("word 'hello' is removed from Trie structure")
    trie.remove("hello")

    Then("Trie contains only 'health'")
    assert(!trie.contains("hello"))
    assert(trie.contains("health"))
  }

  it should "remove existing word from its structure and doesn't affect another words " +
    "(when some word is part of some other longer word and that longer word is removed)" in {

    Given("Trie with added words 'ant', 'antenna'")
    trie.append("antenna")
    trie.append("ant")

    When("longer word is removed from Trie structure")
    trie.remove("antenna")

    Then("shorter word exists in structure")
    assert(trie.contains("antenna") === false)
    assert(trie.contains("ant") === true)
  }

  it should "remove existing word from its structure and doesn't affect another words " +
    "(when some shorter word is part of some other longer word and that shorter word is removed)" in {

    Given("Trie with added words 'ant', 'antenna'")
    trie.append("antenna")
    trie.append("ant")

    When("shorter word is removed from Trie structure")
    trie.remove("ant")

    Then("shorter isn't contained in Trie and longer word exists in Trie")
    assert(trie.contains("ant") === false)
    assert(trie.contains("antenna") === true)
    //TODO check nodes also
  }

  it should "suggest nothing for empty string" in {
    Given("Trie with some added word")
    trie.append("hello")

    When("empty string is passed for suggestion")
    val suggestions = trie.suggest("")

    Then("No suggestion is returned")
    assert(suggestions === List.empty)
  }

}
