package com.korest.algorithm.tree

import scala.util.Random

object BinarySearchTreeTest {

  val intComparator = (a: Int, b: Int) => (if(a > b) 1 else if (a < b) -1 else 0)
  val testData = Array(3, 5, 4, 1, 0, 2)

  def main(args: Array[String]) {
    testAddCorrect
    testAdd
    testFind
    testRemoveCorrect
    testRemove
  }

  def testAddCorrect = {
    val tree = new BinarySearchTree[Int](intComparator)
    for(i <- testData) {
      tree.add(i)
    }

    for(i <- testData) {
      if (!tree.find(testData(i))) {
        println("Failed to find element: " + i)
      }
    }
  }

  def testAdd = {
    val n = 1000000
    var time: Long = 0
    val tree = new BinarySearchTree[Int](intComparator)
    val startTime = System.nanoTime()
    for (i <- 1 to n) {
      val start = System.nanoTime()
      tree.add(Random.nextInt(n))
      time += (System.nanoTime() - start)
    }

    println(f"Binary search tree $n inserts take " + (System.nanoTime() - startTime) / (1e6))
    println(f"Binary search tree average insert takes " + time / (1e6 * n))
  }

  def testFind = {
    val n = 1000000
    var time: Long = 0
    val tree = new BinarySearchTree[Int](intComparator)

    for (i <- 1 to n) {
      tree.add(Random.nextInt(n))
    }

    val startTime = System.nanoTime()
    for (i <- 1 to n) {
      val start = System.nanoTime()
      tree.find(Random.nextInt(n))
      time += (System.nanoTime() - start)
    }

    println(f"Binary search tree $n finds take " + (System.nanoTime() - startTime) / (1e6))
    println(f"Binary search tree average find takes " + time / (1e6 * n))
  }

  def testRemove = {
    val n = 1000000
    var time: Long = 0
    val tree = new BinarySearchTree[Int](intComparator)

    for (i <- 1 to n) {
      tree.add(Random.nextInt(n))
    }

    val startTime = System.nanoTime()
    for (i <- 1 to n) {
      val start = System.nanoTime()
      tree.remove(Random.nextInt(n))
      time += (System.nanoTime() - start)
    }

    println(f"Binary search tree $n removes take " + (System.nanoTime() - startTime) / (1e6))
    println(f"Binary search tree average remove takes " + time / (1e6 * n))
  }

  def testRemoveCorrect = {
    val tree = new BinarySearchTree[Int](intComparator)
    for(i <- testData) {
      tree.add(i)
    }

    tree.remove(testData(0))
    tree.remove(testData(3))
    tree.remove(testData(4))

    if(tree.find(testData(0))) {
      println("Failed element should be removed: 0")
    }
    if(tree.find(testData(3))) {
      println("Failed element should be removed: 0")
    }
    if(tree.find(testData(4))) {
      println("Failed element should be removed: 0")
    }
  }
}
