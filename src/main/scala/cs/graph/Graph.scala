package cs.graph

import scala.collection.immutable.{List, Map}

/*
  Model of directed graph. This is immutable structure. It can be converted from MutableGraph.
 */
class Graph(val vertices: Map[Vertex, List[(Vertex, Int)]]) {

  def neighbours(v: Vertex): Option[List[(Vertex, Int)]] = vertices.get(v)

}
