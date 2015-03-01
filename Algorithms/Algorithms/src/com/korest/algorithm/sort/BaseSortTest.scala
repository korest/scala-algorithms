package com.korest.algorithm.sort

import com.korest.algorithm._

import scala.util.Random

abstract class BaseSortTest {

  def testSort(n: Int, method: Array[Int] => Unit, title: String) = {
    var time: Long = 0
    val repeat = 101
    for (i <- 1 to repeat) {
      val arr = Array.fill(n)(Random.nextInt(n))
      val start = System.nanoTime()
      method(arr)
      if (i > 1) {
        time += (System.nanoTime() - start)
      }
      if (!isIntArraySorted(arr)) {
        println("Not sorted: " + arr.mkString(" "))
      }
    }
    println(f"$title $n: " + time / (1e6 * (repeat - 1)))
  }

}
