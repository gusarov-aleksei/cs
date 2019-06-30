package cs.graph

import org.scalatest.{BeforeAndAfterEach, FunSuite, GivenWhenThen, Matchers}

import scala.collection.mutable

class GraphOpsTest extends FunSuite
  with GivenWhenThen with Matchers {

  test("Breadth first search. Graph has an ability to be traversed in breadth") {
    val g = new MutableGraph
    g.addEdge("1", "2").addEdge("1", "5").addEdge("1","3").addEdge("2","4").addEdge("5","4").addEdge("5","6").
      addEdge("1","7").addEdge("7","8").addEdge("7","9").addEdge("4","10").addEdge("10","11").addEdge("10","12")
    val expected = List(Vertex(1), Vertex(2), Vertex(3), Vertex(5), Vertex(7), Vertex(4), Vertex(6), Vertex(8), Vertex(9), Vertex(10), Vertex(11), Vertex(12))
    //1. first are neighbors of Vertex(1): Vertex(2), Vertex(3), Vertex(5), Vertex(7)
    //2. the next are neighbors of Vertex(2): Vertex(4) (v4 will be traversed after v3, v5, v7)
    //3. Vertex(3) has no neighbors
    //4. the next are neighbors of Vertex(5): Vertex(6)
    //5. further vertices are neighbors of Vertex(7): Vertex(8), Vertex(9)
    //6. Vertex(4) has Vertex(10)
    //7. Vertex(10) has Vertex(11), Vertex(12)
    assert(bfs(Vertex(1), g.toGraph.vertices) === expected)
  }

  test("Breadth first search. There is no path for empty Graph") {
    val g = new Graph(Map())
    bfs(Vertex(1), g.vertices) should be (empty)
  }

  test("Breadth first search. There is no path for Graph without requested Vertex") {
    val g = new MutableGraph
    g.addEdge("1", "2").addEdge("1", "5").addEdge("1","3").addEdge("2","4").addEdge("5","4").addEdge("5","6").
      addEdge("1","7").addEdge("7","8").addEdge("7","9").addEdge("4","10").addEdge("10","11").addEdge("10","12")

    bfs(Vertex("100"), g.toGraph.vertices) should be (empty)
  }

  test("Breadth first search. Graph has an ability to be traversed in breadth till some 'to' destination") {
    val g = new MutableGraph
    val from = Vertex(1)
    val to = Vertex(6)
    g.addEdge("1", "2").addEdge("1", "5").addEdge("1","3").addEdge("2","4").addEdge("5","4").addEdge("5","6").
      addEdge("1","7").addEdge("7","8").addEdge("7","9").addEdge("4","10").addEdge("10","11").addEdge("10","12")
    val expected = List(Vertex(1), Vertex(2), Vertex(3), Vertex(5), Vertex(7), Vertex(4), Vertex(6))

    bfs(from, to, g.toGraph.vertices) should contain theSameElementsInOrderAs expected
  }

  test("Depth first search. Graph has an ability to be traversed in depth") {
    val g = new MutableGraph
    g.addEdge("1", "2").addEdge("1", "5").addEdge("1","3").addEdge("2","4").addEdge("5","4").addEdge("5","6").
      addEdge("1","7").addEdge("7","8").addEdge("7","9").addEdge("4","10").addEdge("10","11").addEdge("10","12").
      addEdge("2","13").addEdge("13","14").addEdge("13","15").addEdge("2","16")
    val expected = List(Vertex(1), Vertex(7), Vertex(9), Vertex(8), Vertex(5), Vertex(6), Vertex(4),
      Vertex(10), Vertex(12), Vertex(11), Vertex(3), Vertex(2), Vertex(16), Vertex(13), Vertex(15), Vertex(14))

    dfs(Vertex(1), g.toGraph.vertices) should contain theSameElementsInOrderAs expected
  }

  test("Dijkstra short path. Algorithm returns empty for given empty graph. Vertex isn't added to graph") {
    val v1 = Vertex(1)
    val g = new MutableGraph
    val (distance, path) = dijkstra(v1, g.toGraph)

    distance should be (empty)
    path should be (empty)
  }

  test("Dijkstra short path. Algorithm returns empty for graph with only one Vertex") {
    val v1 = Vertex(1)
    val g = new MutableGraph
    g.addVertex(v1)

    val (distance, path) = dijkstra(v1, g.toGraph)

    distance should contain theSameElementsAs Map(Vertex(1) -> 0)
    path should be (empty)
  }

  test("Dijkstra short path. Algorithm calculates short path from source 'from' to each vertex of directed graph(with cycle). Case with all edges weight = 1") {
    val g = new MutableGraph
    g.addEdge("1", "2").addEdge("1", "5").addEdge("1","3").addEdge("2","4").addEdge("5","4").addEdge("5","6").
      addEdge("1","7").addEdge("7","8").addEdge("7","9").addEdge("4","10").addEdge("10","11").addEdge("10","12").
      addEdge("1", "4")

    val (distance, path) = dijkstra(Vertex(1), g.toGraph)

    distance should contain theSameElementsAs
      Map(Vertex(12) -> 3, Vertex(8) -> 2, Vertex(4) -> 1, Vertex(11) -> 3, Vertex(9) -> 2, Vertex(5) -> 1, Vertex(10) -> 2, Vertex(6) -> 2, Vertex(1) -> 0, Vertex(2) -> 1, Vertex(7) -> 1, Vertex(3) -> 1)
    path should contain theSameElementsAs
      Map(Vertex(12) -> Vertex(10), Vertex(8) -> Vertex(7), Vertex(4) -> Vertex(1), Vertex(11) -> Vertex(10), Vertex(9) -> Vertex(7), Vertex(5) -> Vertex(1), Vertex(10) -> Vertex(4), Vertex(6) -> Vertex(5), Vertex(2) -> Vertex(1), Vertex(7) -> Vertex(1), Vertex(3) -> Vertex(1))
  }

  test("Dijkstra short path. Case with different edges weight of directed graph(without cycle). Case with different edges weight") {
    val g = new MutableGraph
    g.add(Seq(("A","B",1), ("B","C",2), ("C", "D", 1), ("A", "D", 2), ("A", "E", 2), ("A", "F" , 3)))

    val (distance, path) = dijkstra(Vertex("A"), g.toGraph)

    distance should contain theSameElementsAs
      Map(Vertex("E") -> 2, Vertex("F") -> 3, Vertex("A") -> 0, Vertex("B") -> 1, Vertex("C") -> 3, Vertex("D") -> 2)
    path should contain theSameElementsAs
      Map(Vertex("E") -> Vertex("A"), Vertex("F") -> Vertex("A"), Vertex("B") -> Vertex("A"), Vertex("C") -> Vertex("B"), Vertex("D") -> Vertex("A"))
  }

  test("Dijkstra short path. Case with different edges weight of directed graph(with cycle in edge 'A'). Case with different edges weight") {
    val g = new MutableGraph
    g.add(Seq(("A","B",1), ("B","C",2), ("C", "D", 1), ("D", "A", 2), ("A", "E", 2), ("A", "F" , 3)))

    val (distance, path) = dijkstra(Vertex("A"), g.toGraph)

    distance should contain theSameElementsAs
      Map(Vertex("E") -> 2, Vertex("F") -> 3, Vertex("A") -> 0, Vertex("B") -> 1, Vertex("C") -> 3, Vertex("D") -> 4)
    path should contain theSameElementsAs
      Map(Vertex("E") -> Vertex("A"), Vertex("F") -> Vertex("A"), Vertex("B") -> Vertex("A"), Vertex("C") -> Vertex("B"), Vertex("D") -> Vertex("C"))
  }

  test("Dijkstra short path. Algorithm calculates short path from source to each vertex") {
    val g = new MutableGraph
    g.addEdge("1", "2", 1).addEdge("2", "3", 2)
    g.addEdge("3", "5", 3).addEdge("1", "4", 20)
    g.addEdge("4", "5", 30).addEdge("1", "5", 100)

    val (distance, path) = dijkstra(Vertex(1), g.toGraph)

    distance should contain theSameElementsAs
      Map(Vertex(5) -> 6, Vertex(1) -> 0, Vertex(2) -> 1, Vertex(3) -> 3, Vertex(4) -> 20)
    path should contain theSameElementsAs
      Map(Vertex(2) -> Vertex(1), Vertex(5) -> Vertex(3), Vertex(4) -> Vertex(1), Vertex(3) -> Vertex(2))
  }

  test("Dijkstra short path. Algorithm calculates short path for all vertices of undirected graph starting from 'from' Vertex. Undirected graph.") {
    val g = new MutableGraph
    g.addUndirectedEdge(Vertex("A"), Vertex("B"), 6).
      addUndirectedEdge(Vertex("B"), Vertex("C"), 5).
      addUndirectedEdge(Vertex("C"), Vertex("E"), 5).
      addUndirectedEdge(Vertex("A"), Vertex("D"), 1).
      addUndirectedEdge(Vertex("B"), Vertex("E"), 2).
      addUndirectedEdge(Vertex("D"), Vertex("B"), 2).
      addUndirectedEdge(Vertex("D"), Vertex("E"), 1)

    val (distance, path) = dijkstra(Vertex("A"), g.toGraph)

    distance should contain theSameElementsAs
      Map(Vertex("A") -> 0, Vertex("B") -> 3, Vertex("C") -> 7, Vertex("D") -> 1, Vertex("E") -> 2)
    path should contain theSameElementsAs
      Map(Vertex("D") -> Vertex("A"), Vertex("C") -> Vertex("E"), Vertex("E") -> Vertex("D"), Vertex("B") -> Vertex("D"))
  }

}
