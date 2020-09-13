# Computer Science and Scala situations. 

This repository is Computer Science(CS) practicing in Scala programming language.
It shows solving basic computational problems depending on operations applied to data
and consists of some occasional attempts of subject understanding. This is kind of try to enter into Scala and CS fields.

Why Scala? Any other programming language is applicable for such fundamental concepts but Scala choice happened because of its reasonable hype and accurate ethical intention (why not?).

### Main folder

Main folder demonstrates several algorithms and data structures reflected in Scala language and each data structure performs reading, writing operations with its own, specific time and space complexity. 
Particular situations and circumstances require particular data structure and algorithm to operate with data.

Here are few implementations and its some characteristics. 
For example, both [Tree](./src/main/scala/cs/tree/avl/AVLTree.scala) and [MinHeap](./src/main/scala/cs/heap/MinHeap.scala) perform write and remove operation with `O(log n)` time complexity. Read operation retrieves min element in `O(1)` time for Heap and in `O(log n)` time in case of Tree. Heap is good for min element getting while Tree is good for elements searching. 

Also codebase contains [Graph](./src/main/scala/cs/graph/Graph.scala) example and some traversal [operations](./src/main/scala/cs/graph/GraphOps.scala) on it. 

### Test folder

Besides main implementation, there are [tests](./src/test/scala/cs) for implementation acceptance.

Some experiments with [Trie](./src/main/scala/cs/trie/mutable/Trie.scala) data structure could be found in [`TrieExperiment`](./src/test/scala/cs/trie/mutable/TrieExperiment.scala) class: there is utility library [Java Object Layout](https://openjdk.java.net/projects/code-tools/jol/) is used to measure memory size consumed by data structure.
It is taken for comparison with brute force approach of reading data (words in dictionary). Memory and time observations were put in that class for notice.

Measure tool would be probably quite useful in estimation and planning of resource consumption by JVM.

### Summary to resource
This repo is connected with CS studying. So, to get more details of scientific foundations and research results, Algorithms [part 1](https://www.coursera.org/learn/algorithms-part1), [part 2](https://www.coursera.org/learn/algorithms-part2) by Princeton University course (with Java language) could be used.
