package cs.graph

import scala.collection.mutable

/**
  * Some operations on Graph: Breadth first traverse, Depth first traverse, Dijkstra short path algorithm
  */
trait GraphOps {

  /**
    * Evaluates path using breadth first search for given vertices starting from 'from' Vertex
    * Until all Vertices be reached.
    * @param from - traversing starts from this Vertex
    * @param vertices - given vertices for operation applying
    * @return
    */
  def bfs(from: Vertex, vertices: Map[Vertex, List[(Vertex, Int)]]): List[Vertex] = vertices.get(from) match {
    case None => List.empty
    case Some(_) => applyBfs(from, vertices)
  }

  private def applyBfs(from: Vertex, vertices:  Map[Vertex, List[(Vertex, Int)]]): List[Vertex] = {
    val visited = new mutable.MutableList[Vertex]
    val toVisit = new mutable.Queue[Vertex]
    toVisit.enqueue(from)
    while (!toVisit.isEmpty) {
      val vertex = toVisit.dequeue()
      if (!visited.contains(vertex)) {
        visited.+=(vertex)
        vertices.get(vertex) match {
          case None =>
          case Some(neighbors) => neighbors.map(e => e._1).foreach(v => toVisit.enqueue(v))
        }
      }
    }
    visited.toList
  }

  /**
    * Evaluates path using breadth first search for given vertices starting from 'from' Vertex
    * It stops when  to' Vertex is being reached
    * @param from - traversing starts from this Vertex
    * @param to - traversing ends at this Vertex
    * @param vertices
    * @return
    */
  def bfs(from: Vertex, to: Vertex, vertices:  Map[Vertex, List[(Vertex, Int)]]): List[Vertex] = {
    val visited = new mutable.MutableList[Vertex]
    val toVisit = new mutable.Queue[Vertex]
    toVisit.enqueue(from)
    while (!toVisit.isEmpty) {
      val vertex = toVisit.dequeue()
      if (!visited.contains(vertex)) {
        visited.+=(vertex)
        if (vertex.equals(to))
          return visited.toList
        vertices.get(vertex) match {
          case None =>
          case Some(neighbors) => neighbors.map(e => e._1).foreach(v => toVisit.enqueue(v))
        }
      }
    }
    visited.toList
  }

  /**
    * Evaluates path using depth first traversing for given vertices starting from 'from' Vertex
    * @param from
    * @param vertices
    * @return
    */
  def dfs(from: Vertex, vertices:  Map[Vertex, List[(Vertex, Int)]]): List[Vertex] =  {
    val visited = new mutable.MutableList[Vertex]
    val toVisit = new mutable.ArrayStack[Vertex]
    toVisit.push(from)
    while (!toVisit.isEmpty) {
      val vertex = toVisit.pop()
      if (!visited.contains(vertex)) {
        visited.+=(vertex)
        vertices.get(vertex) match {
          case None =>
          case Some(neighbors) => neighbors.map(e => e._1).foreach(v => toVisit.push(v))
        }
      }
    }
    visited.toList
  }

  /**
    * Evaluates short path for all vertices in presented graph starting from "from" Vertex
    * @param from - traversing starts from this Vertex
    * @param graph - graph for evaluation
    * @return (Map[Vertex, Int], Map[Vertex, Vertex]) where
    *         Map[Vertex, Int] is distance weight to each traversed Vertex
    *         Map[Vertex, Vertex] is path showing traverse to each previous Vertex: key - next Vertex in path, value - previous Vertex in path
    */
  def dijkstra(from: Vertex, graph: Graph) : (Map[Vertex, Int], Map[Vertex, Vertex]) = graph.vertices.get(from) match {
    case None => (Map(), Map())
    case Some(_) => applyDijkstra(from, graph)
  }

  private def applyDijkstra(from: Vertex, graph: Graph) : (Map[Vertex, Int], Map[Vertex, Vertex]) = {
    val visited = new mutable.HashSet[Vertex]
    val path = new mutable.HashMap[Vertex, Vertex]
    val distances = new mutable.HashMap[Vertex, Int]
    val toVisit = new mutable.HashMap[Vertex, Int] //TODO to replace with PriorityQueue(or Heap which has ability to change queued elements priority)
    distances.put(from, 0)
    toVisit.put(from, 0)//value is distance. it used for finding Vertex with min distance
    while (!toVisit.isEmpty) {//TODO to get rid from toVisit iterating via graph.vertices.get(v._1)
      val v = toVisit.minBy(_._2)//retrieve Vertex with min value
      toVisit.remove(v._1)
      if (!visited.contains(v._1)) {
        visited.+=(v._1)
        graph.neighbours(v._1) match {
          case None =>
          case Some(neighbours) =>
            neighbours.foreach(n => {
              toVisit.put(n._1, Int.MaxValue)
              val updated = distances.getOrElse(v._1, Int.MaxValue) + n._2 //TODO what if MaxValue + some value > MaxValue
              if (updated < distances.getOrElse(n._1, Int.MaxValue)) {
                distances.put(n._1, updated)
                toVisit.put(n._1, updated)
                path.put(n._1, v._1)
              }
            })
        }
      }
    }
    (distances.toMap, path.toMap)
  }
}
