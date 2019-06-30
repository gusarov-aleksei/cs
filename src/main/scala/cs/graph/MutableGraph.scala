package cs.graph

import scala.collection.mutable
import scala.collection.mutable.{HashMap, HashSet, Set}


/*
  Model of directed graph. MutableGraph is used for data initialisation and can be converted immutable structure.
 */
class MutableGraph {

  private val _vertices: mutable.HashMap[Vertex, Set[(Vertex, Int)]] = new HashMap[Vertex, Set[(Vertex, Int)]]

  final def vertices = _vertices

  def addVertex(v: Vertex): MutableGraph = {
    require(v != null, "Vertex must be non-null")
    vertices.getOrElseUpdate(v, new HashSet[(Vertex, Int)])
    this
  }

  def addEdge(from: Vertex, to: Vertex): MutableGraph = {
    addEdge(from, to, 1)
  }

  def addEdges(edges: Seq[(Vertex, Vertex, Int)] ): MutableGraph = {
    edges.foreach(e => addEdge(e._1, e._2, e._3))
    this
  }

  def add(edges: Seq[(String, String, Int)] ): MutableGraph = {
    edges.foreach(e => addEdge(e._1, e._2, e._3))
    this
  }

  //add edge with from -> to direction
  def addEdge(from: Vertex, to: Vertex, weight: Int): MutableGraph = {
    require(from != null, "from Vertex must be non-null")
    require(to != null, "to Vertex must be non-null")
    require(from != to, "Vertices must be different")
    vertices.getOrElseUpdate(from, new HashSet[(Vertex, Int)]).+=((to, weight))
    addVertex(to)
  }

  def addEdge(from: String, to: String): MutableGraph = {
    addEdge(from, to, 1)
  }

  def addEdge(from: String, to: String, weight: Int): MutableGraph = {
    addEdge(Vertex(from), Vertex(to), weight: Int)
  }

  def addUndirectedEdge(v1: Vertex, v2: Vertex, weight: Int): MutableGraph = {
    require(v1 != null, "v1 Vertex must be non-null")
    require(v2 != null, "v2 Vertex must be non-null")
    require(v1 != v2, "Vertices must be different")
    vertices.getOrElseUpdate(v1, new HashSet[(Vertex, Int)]).+=((v2, weight))
    vertices.getOrElseUpdate(v2, new HashSet[(Vertex, Int)]).+=((v1, weight))
    this
  }

  def neighbours(v: Vertex): Option[Set[(Vertex, Int)]] = vertices.get(v)

  //sorting is using here for visualisation convenience and simplicity of testing
  private def sortByValue(v1: (Vertex, Int),v2: (Vertex, Int)) : Boolean = v1._1.value < v2._1.value

  //converts to immutable graph
  def toGraph: Graph = new Graph(vertices.map({case(k, v) => (k, v.toList.sortWith(sortByValue))}).toMap)

}
