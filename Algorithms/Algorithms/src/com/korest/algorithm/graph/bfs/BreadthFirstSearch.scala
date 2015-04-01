package com.korest.algorithm.graph.bfs

import scala.collection.mutable

object BreadthFirstSearch extends App {

  val adj = Map[String, Array[String]](
    ("a", Array("b", "c")),
    ("b", Array("d", "c")),
    ("c", Array("z", "x")),
    ("d", Array("a", "x")))

  bfs("a", adj)

  def bfs(start: String, adj: Map[String, Array[String]]) = {
    val marked = new mutable.HashSet[String]
    marked += start
    val queue = new mutable.Queue[String]
    queue += start
    val paths = new mutable.LinkedHashMap[String, String]
    paths += "a" -> "a"
    while(queue.length > 0) {
      val vertex = queue.dequeue()
      val path = paths(vertex)
      if(adj.contains(vertex)) {
        for (v <- adj(vertex)) {
          if (!marked.contains(v)) {
            queue.enqueue(v);
            marked += v
            paths += v -> (path + v)
          }
        }
      }
    }

    println(paths)
  }

}
