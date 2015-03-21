package com.korest.algorithm.skiplist

import scala.util.Random

object SkipListTest {

  val testData = Array(3, 5, 4, 1, 0, 2, 0, 6, 8, 7)

  def main (args: Array[String]) {
    testInsertCorrect
    testInsert
    testGet
    testRemoveCorrect
    testRemove
  }

  def testInsert = {
    val n = 1000000
    var time: Long = 0
    val sl = new SkipList[Int, Int]()
    val startTime = System.nanoTime()
    for (i <- 1 to n) {
      val key = Random.nextInt(n)
      val value = Random.nextInt(n)
      val start = System.nanoTime()
      sl.put(key, value)
      time += (System.nanoTime() - start)
    }

    println(f"Skip list $n inserts take " + (System.nanoTime() - startTime) / (1e6))
    println(f"Skip list average insert takes " + time / (1e6 * n))
  }

  def testInsertCorrect = {
    val sl = new SkipList[Int, Int]()
    for(i <- testData) {
      sl.put(i, i)
    }

    for(i <- testData) {
      if (sl.get(testData(i)).isEmpty) {
        println("Failed to get element: " + i)
      }
    }
  }

  def testGet = {
    val n = 1000000
    var time: Long = 0
    val sl = new SkipList[Int, Int]()
    for (i <- 1 to n) {
      val key = Random.nextInt(n)
      val value = Random.nextInt(n)
      sl.put(key, value)
    }

    val startTime = System.nanoTime()
    for (i <- 1 to n) {
      val start = System.nanoTime()
      sl.get(Random.nextInt(n))
      time += (System.nanoTime() - start)
    }


    println(f"Skip list $n gets take " + (System.nanoTime() - startTime) / (1e6))
    println(f"Skip list average get takes " + time / (1e6 * n))
  }
  
  def testRemove = {
    val n = 1000000
    var time: Long = 0
    val sl = new SkipList[Int, Int]
    for (i <- 1 to n) {
      val key = Random.nextInt(n)
      val value = Random.nextInt(n)
      sl.put(key, value)
    }

    val startTime = System.nanoTime()
    for (i <- 1 to n) {
      val start = System.nanoTime()
      sl.remove(Random.nextInt(n))
      time += (System.nanoTime() - start)
    }

    println(f"Skip list $n removes take " + (System.nanoTime() - startTime) / (1e6))
    println(f"Skip list average remove takes " + time / (1e6 * n))
  }

  def testRemoveCorrect = {
    val sl = new SkipList[Int, Int]
    for(i <- testData) {
      sl.put(i, i)
    }

    sl.remove(testData(0))
    sl.remove(testData(4))
    sl.remove(testData(8))

    if(sl.get(testData(0)).nonEmpty) {
      println("Failed element should be removed: " + testData(0))
    }
    if(sl.get(testData(4)).nonEmpty) {
      println("Failed element should be removed: " + testData(4))
    }
    if(sl.get(testData(8)).nonEmpty) {
      println("Failed element should be removed: " + testData(8))
    }
  }
  
}