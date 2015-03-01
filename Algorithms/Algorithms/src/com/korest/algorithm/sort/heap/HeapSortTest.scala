package com.korest.algorithm.sort.heap

import com.korest.algorithm.sort.BaseSortTest

object HeapSortTest extends BaseSortTest {

  def main(args: Array[String]): Unit = {
    testHeapSort10
    testHeapSort1000
  }

  def testHeapSort10 = {
    testSort(10, HeapSort.sortInt, "Heap sort")
  }

  def testHeapSort1000 = {
    testSort(1000, HeapSort.sortInt, "Heap sort")
  }

}
