package com.korest.algorithm.hashtable

import scala.util.Random

object HashTableTest {

  def main (args: Array[String]) {
    //testInsert
    testInsertWithResize
    testGet
    testRemove
  }

  def testInsert = {
    val n = 1000000
    var time: Long = 0
    val h = new HashTable[Int, Int](n)
    val startTime = System.nanoTime()
    for (i <- 1 to n) {
      val key = Random.nextInt(n)
      val value = Random.nextInt(n)
      val start = System.nanoTime()
      h.put(key, value)
      time += (System.nanoTime() - start)
    }

    println(f"Hash table $n inserts take " + (System.nanoTime() - startTime) / (1e6))
    println(f"Hash table average insert takes " + time / (1e6 * n))
  }

  def testInsertWithResize = {
    val n = 1000000
    var time: Long = 0
    val h = new HashTable[Int, Int]()
    val startTime = System.nanoTime()
    for (i <- 1 to n) {
      val key = Random.nextInt(n)
      val value = Random.nextInt(n)
      val start = System.nanoTime()
      h.put(key, value)
      time += (System.nanoTime() - start)
    }

    println(f"Hash table $n inserts while growing take " + (System.nanoTime() - startTime) / (1e6))
    println(f"Hash table average insert while growing takes " + time / (1e6 * n))
  }

  def testGet = {
    val n = 1000000
    var time: Long = 0
    val h = new HashTable[Int, Int]()
    for (i <- 1 to n) {
      val key = Random.nextInt(n)
      val value = Random.nextInt(n)
      h.put(key, value)
    }

    val startTime = System.nanoTime()
    for (i <- 1 to n) {
      val start = System.nanoTime()
      h.get(Random.nextInt(n))
      time += (System.nanoTime() - start)
    }


    println(f"Hash table $n gets take " + (System.nanoTime() - startTime) / (1e6))
    println(f"Hash table average get takes " + time / (1e6 * n))
  }

  def testRemove = {
    val n = 1000000
    var time: Long = 0
    val h = new HashTable[Int, Int]()
    for (i <- 1 to n) {
      val key = Random.nextInt(n)
      val value = Random.nextInt(n)
      h.put(key, value)
    }

    val startTime = System.nanoTime()
    for (i <- 1 to n) {
      val start = System.nanoTime()
      h.remove(Random.nextInt(n))
      time += (System.nanoTime() - start)
    }

    println(f"Hash table $n removes take " + (System.nanoTime() - startTime) / (1e6))
    println(f"Hash table average remove takes " + time / (1e6 * n))
  }

}
