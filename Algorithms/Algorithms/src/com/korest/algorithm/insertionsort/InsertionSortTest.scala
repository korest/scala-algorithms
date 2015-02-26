package com.korest.algorithm.insertionsort

import com.korest.algorithm._

import scala.util.Random

object InsertionSortTest {

  def main(args: Array[String]): Unit = {
    testInsertionSort10
    testBinaryInsertionSort10
    testInsertionSort1000
    testBinaryInsertionSort1000
  }

  def testInsertionSort10 = {
    testSort(10, InsertionSort.sortInt, "Insertion sort")
  }

  def testInsertionSort1000 = {
    testSort(1000, InsertionSort.sortInt, "Insertion sort")
  }

  def testBinaryInsertionSort10 = {
    testSort(10, InsertionSort.sortBinaryInt, "Binary Insertion sort")
  }

  def testBinaryInsertionSort1000 = {
    testSort(1000, InsertionSort.sortBinaryInt, "Binary Insertion sort")
  }
  
  def testSort(n: Int, method: Array[Int] => Unit, title: String) = {
    var time: Long = 0
    for (i <- 1 to 1000) {
      val arr = Array.fill(n)(Random.nextInt(n))
      val start = System.nanoTime()
      method(arr)
      time += (System.nanoTime() - start)
      if (!isIntArraySorted(arr)) {
        println("Not sorted: " + arr.mkString(" "))
      }
    }
    println(f"$title $n: " + time / 1e9)
  }

}