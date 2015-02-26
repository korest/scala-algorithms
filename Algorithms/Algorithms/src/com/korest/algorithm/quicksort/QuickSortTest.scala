package com.korest.algorithm.quicksort

import com.korest.algorithm._

import scala.util.Random

object QuickSortTest {

  def main(args: Array[String]): Unit = {
    testQuickSort10
    testQuickSort1000
  }
  
  def testQuickSort10 = {
    testSort(10, QuickSort.quickSortInt, "Quick sort")
  }
  
  def testQuickSort1000 = {
    testSort(1000, QuickSort.quickSortInt, "Quick sort")
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