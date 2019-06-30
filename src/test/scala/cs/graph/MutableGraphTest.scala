package cs.graph

import org.scalatest.{BeforeAndAfterEach, FunSuite, GivenWhenThen}
import org.scalatest._

import scala.collection.{immutable, mutable}

class MutableGraphTest extends FunSuite
  with GivenWhenThen with Matchers {

  test("Empty mutable graph contains empty set of Vertices and Edges") {
    val g = new MutableGraph
    assert(g.vertices == mutable.HashMap.empty, "No vertices in empty graph")
  }

  test("Graph returns None for not added Vertex") {
    val g= new MutableGraph
    val v1 = Vertex(1)
    assert(g.neighbours(v1) == None, "None for non-existing Vertex in Graph")
  }

  test("Null is illegal for addVertex method") {
    val g= new MutableGraph
    val caught = intercept[IllegalArgumentException]{
      g.addVertex(null)
    }
    assert(caught.getMessage == "requirement failed: Vertex must be non-null")
  }

  test("Null is illegal for addEdge method first argument") {
    val g= new MutableGraph
    val caught = intercept[IllegalArgumentException]{
      g.addEdge(null, Vertex(1), 1)
    }
    assert(caught.getMessage == "requirement failed: from Vertex must be non-null")
  }

  test("Null is illegal for addEdge method second argument") {
    val g= new MutableGraph
    val caught = intercept[IllegalArgumentException]{
      g.addEdge(Vertex(3), null, 1)
    }
    assert(caught.getMessage == "requirement failed: to Vertex must be non-null")
  }

  test("Null is illegal for addUndirectedEdge method first argument") {
    val g= new MutableGraph
    val caught = intercept[IllegalArgumentException]{
      g.addUndirectedEdge(null, Vertex(1), 1)
    }
    assert(caught.getMessage == "requirement failed: v1 Vertex must be non-null")
  }

  test("Null is illegal for addUndirectedEdge method second argument") {
    val g= new MutableGraph
    val caught = intercept[IllegalArgumentException]{
      g.addUndirectedEdge(Vertex(1), null, 1)
    }
    assert(caught.getMessage == "requirement failed: v2 Vertex must be non-null")
  }

  test("It is not possible to add undirected Edge with the same Vertices") {
    val g= new MutableGraph
    val caught = intercept[IllegalArgumentException]{
      g.addUndirectedEdge(Vertex(1), Vertex(1), 1)
    }
    assert(caught.getMessage == "requirement failed: Vertices must be different")
  }

  test("It is possible to add undirected Edge with different Vertices") {
    val g= new MutableGraph
    g.addUndirectedEdge(Vertex(1), Vertex(2), 1)
    g.vertices should contain theSameElementsAs Map(Vertex(2) -> Set((Vertex(1),1)), Vertex(1) -> Set((Vertex(2),1)))
  }

  test("Graph must contains exactly one Vertex in case of adding the same Edges(wih the same Vertex) several times") {
    val g= new MutableGraph
    g.addUndirectedEdge(Vertex(1), Vertex(2), 1)
    g.addUndirectedEdge(Vertex(1), Vertex(2), 1)
    g.addUndirectedEdge(Vertex(2), Vertex(1), 1)
    g.addUndirectedEdge(Vertex(2), Vertex(1), 1)
    g.addUndirectedEdge(Vertex(1), Vertex(2), 1)
    g.vertices should contain theSameElementsAs Map(Vertex(2) -> Set((Vertex(1),1)), Vertex(1) -> Set((Vertex(2),1)))
  }

  test("Graph has an ability to add only Vertex. Graph with only one Vertex contains no neighbors") {
    val g= new MutableGraph
    val v1 = Vertex(1)
    g.addVertex(v1)
    assert(g.neighbours(v1) == Some(Set()), "Vertex with empty neighbors")
  }

  test("It is possible to add the same Vertex only once. Graph doesn't contain duplicates of the same Vertex") {
    val g= new MutableGraph
    val v1 = Vertex(1)
    g.addVertex(v1)
    g.addVertex(v1)
    g.addVertex(v1)
    g.vertices should contain theSameElementsAs Map(Vertex(1) -> Set())
  }

  test("Graph is able to add Edge. Edges are modeled as Vertex -> Set((Vertex, weight), (Vertex, weight))") {
    val g= new MutableGraph
    val v1 = Vertex(1)
    val v2 = Vertex(2)
    g.addEdge(v1, v2)
    assert(g.neighbours(v1) == Some(Set((v2, 1))), "Vertex with one edge with v1->v2")
  }

  test("By default Edge weight is 1") {
    val g = new MutableGraph
    val v1 = Vertex(1)
    val v2 = Vertex(2)
    g.addEdge(v1, v2)
    assert(g.neighbours(v1).isDefined, "Vertex is added to Graph")
    val (vertex, weight) = g.neighbours(v1).get.head
    assert(vertex == v2, "second Vertex of Edge")
    assert(weight == 1, " default weight of Edge")
  }

  test("Graph doesn't contain duplicates in case of adding the same Edge 'from' -> 'to' several time") {
    val g = new MutableGraph
    val v1 = Vertex(1)
    val v2 = Vertex(2)
    g.addEdge(v1, v2).addEdge(v1, v2).addEdge(v1, v2).addEdge(v1, v2).addEdge(v1, v2)
    g.vertices should contain only (v1 -> Set((v2,1)), v2 -> Set())
  }

  test("It is not possible to add Edge with the same Vertices") {
    val g = new MutableGraph
    val v1 = Vertex(10)
    val caught = intercept[IllegalArgumentException]{
      g.addEdge(v1, v1)
    }
    assert(caught.getMessage == "requirement failed: Vertices must be different")
  }

  test("It is not possible to add Edge with equaled Vertices") {
    val g = new MutableGraph
    //Vertices are compared by value. check with new Vertex instance
    val caught = intercept[IllegalArgumentException]{
      g.addEdge(Vertex(10), Vertex(10))
    }
    assert(caught.getMessage == "requirement failed: Vertices must be different")
  }

  test("It is possible to specify Edge weight") {
    val g= new MutableGraph
    val v1 = Vertex(10)
    val v2 = Vertex(30)
    g.addEdge(v1, v2, 50)
    val (vertex, weight) = g.neighbours(v1).get.head
    assert(vertex == v2, "neighbor Vertex")
    assert(weight == 50, "specified weight of Edge")
  }

  test("Mutable Graph has ability to add edges in bulk way using Sequence") {
    val v1 = Vertex(1)
    val v2 = Vertex(2)
    val v3 = Vertex(3)
    val v4 = Vertex(4)
    val g = new MutableGraph
    g.addEdges(Seq((v1, v2, 1), (v1, v3, 1), (v4, v3, 1)))
    g.vertices should contain only (v1 -> Set((v2,1), (v3,1)), v4 -> Set((v3,1)), v2 -> Set(), v3 -> Set())
  }

  test("Mutable Graph has ability to add Edges. Method addEdge stores edge in state of MutableGraph and returns reference to MutableGraph") {
    val g = new MutableGraph
    g.addEdge(Vertex(1), Vertex(2)).addEdge(Vertex(2),Vertex(3)).addEdge(Vertex(1),Vertex(5))
    g.vertices should contain only (Vertex(2) -> Set((Vertex(3),1)), Vertex(5) -> Set(), Vertex(1) -> Set((Vertex(5),1), (Vertex(2),1)), Vertex(3) -> Set())
  }

  test("Mutable Graph has ability to add separated Vertex.") {
    val g = new MutableGraph
    g.addVertex(Vertex(1))
    g.vertices should contain only (Vertex(1) -> Set())
  }

  test("Immutable Graph can be created from MutableGraph. Internal collections are immutable") {
    val g = new MutableGraph
    g.addEdge(Vertex(1), Vertex(2))
    val vertices = g.toGraph.vertices
    vertices shouldBe a [immutable.Map[_, _]] //Edges are placed into Map
    vertices.values.foreach(v => {v shouldBe a [immutable.List[_]]}) //all Vertex's neighbors are placed in List
  }

  test("Created in bulk way Mutable Graph can be converted into Immutable Graph (redundant test)") {
    val v1 = Vertex(1)
    val v2 = Vertex(2)
    val v3 = Vertex(3)
    val v4 = Vertex(4)
    val g = new MutableGraph
    g.addEdges(Seq((v1, v2, 1), (v1, v3, 1), (v4, v3, 1)))
    g.toGraph.vertices should contain only (v1 -> List((v2,1), (v3,1)), v4 -> List((v3,1)), v2 -> List(), v3 -> List())
  }

  test("Immutable Graph can be created from MutableGraph. All data is the same") {
    val g = new MutableGraph
    g.addEdge("1", "2").addEdge("1", "5").addEdge("1","3").addEdge("2","4").addEdge("5","4").addEdge("5","6").
      addEdge("1","7").addEdge("7","8").addEdge("7","9").addEdge("4","10").addEdge("10","11").addEdge("10","12")
    val expected =  Map(
      Vertex(5) -> List((Vertex(4),1), (Vertex(6),1)),
      Vertex(10) -> List((Vertex(11),1), (Vertex(12),1)),
      Vertex(1) -> List((Vertex(2),1), (Vertex(3),1), (Vertex(5),1), (Vertex(7),1)),
      Vertex(6) -> List(),
      Vertex(9) -> List(),
      Vertex(2) -> List((Vertex(4),1)),
      Vertex(12) -> List(),
      Vertex(7) -> List((Vertex(8),1), (Vertex(9),1)),
      Vertex(3) -> List(),
      Vertex(11) -> List(),
      Vertex(8) -> List(),
      Vertex(4) -> List((Vertex(10),1))
    )
     g.toGraph.vertices should contain theSameElementsAs expected
   }

}
