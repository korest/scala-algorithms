package com.korest.algorithm.sort.heap

import com.korest.algorithm._

object HeapSort {

  // create heap structure array
  def buildHeap[T](arr: Array[T], comparator: (T,T) => Boolean) = {
    val size = (arr.length - 1) / 2
    for(i <- size to 0 by -1) {
      heapify(arr, i, comparator)
    }
  }

  def sort[T](arr: Array[T], comparator: (T,T) => Boolean) = {
    buildHeap(arr, comparator)
    for(i <- (arr.length - 1) to 1 by -1) {
      swap(arr, 0, i)
      heapify(arr, 0, comparator)(i)
    }
  }

  def sortInt(arr: Array[Int]) = {
    sort(arr, intComparator)
  }

  // move element at @index to it's heap position
  def heapify[T](arr: Array[T], index: Int, comparator: (T,T) => Boolean)(implicit length: Int = arr.length): Unit = {
    val leftIndex = 2 * index + 1 // left child
    val rightIndex = 2 * index + 2 // right child
    var largest = index
    if (leftIndex < length && comparator(arr(leftIndex), arr(index))) {
      largest = leftIndex
    }
    if (rightIndex < length && comparator(arr(rightIndex), arr(largest))) {
      largest = rightIndex
    }
    if (largest != index) {
      swap(arr, index, largest)
      heapify(arr, largest, comparator)
    }
  }

}
