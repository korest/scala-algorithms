package com.korest.algorithm.sort.merge

import com.korest.algorithm._

import scala.reflect.ClassTag

object MergeSort {

  def main(args: Array[String]) {
    val arr = Array(5, 3, 2, 1, 1)
    sort(arr, intComparator)
    println(arr.mkString(" "))
  }

  def sort[T: ClassTag](arr: Array[T], comparator: (T, T) => Boolean) = {
    def internalSort(low: Int, high: Int): Unit = {
      if (low < high) {
        val middle = low + (high - low) / 2
        internalSort(low, middle)
        internalSort(middle + 1, high)
        merge(low, middle, high)
      }
    }

    def merge(low: Int, middle: Int, high: Int) = {
      val left = new Array[T](middle - low + 1)
      val right = new Array[T](high - middle)

      for (i <- 0 to left.length - 1) {
        left(i) = arr(low + i)
      }
      for (i <- 0 to right.length - 1) {
        right(i) = arr(middle + i + 1)
      }

      var li = 0
      var ri = 0
      var i = low

      while(li < left.length && ri < right.length) {
        if (comparator(right(ri), left(li))) {
          arr(i) = left(li)
          li = li + 1
        } else {
          arr(i) = right(ri)
          ri = ri + 1
        }
        i = i + 1
      }

      while(li < left.length) {
        arr(i) = left(li)
        i = i + 1
        li = li + 1
      }
    }

    internalSort(0, arr.length - 1)
  }

  def sortInt(arr: Array[Int]) = {
    sort(arr, intComparator)
  }

}
