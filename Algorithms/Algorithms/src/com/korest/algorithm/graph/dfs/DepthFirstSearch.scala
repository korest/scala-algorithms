package com.korest.algorithm.graph.dfs

import scala.collection.mutable

object DepthFirstSearch extends App {

  val adj = Map[String, Array[String]](
    ("a", Array("b", "c")),
    ("b", Array("a", "d", "e")),
    ("c", Array("a", "d", "g")),
    ("d", Array("b", "c", "f", "g")),
    ("e", Array("b", "f")),
    ("f", Array("d", "e", "g")),
    ("g", Array("c", "d", "f")))

  dfs("a", adj)

  def dfs(start: String, adj: Map[String, Array[String]]) = {
    val marked = new mutable.HashSet[String]
    marked += start
    val stack = new mutable.Stack[String]
    stack.push(start)
    val paths = new mutable.LinkedHashMap[String, String]
    paths += "a" -> "a"
    while(stack.length > 0) {
      val vertex = stack.pop()
      val path = paths(vertex)
      if(adj.contains(vertex)) {
        for (v <- adj(vertex)) {
          if (!marked.contains(v)) {
            stack.push(v);
            marked += v
            paths += v -> (path + v)
          }
        }
      }
    }

    println(paths)
  }

}
