package com.korest.algorithm.sort.merge

import com.korest.algorithm.sort.BaseSortTest

object MergeSortTest extends BaseSortTest {

  def main(args: Array[String]): Unit = {
    testMergeSort10
    testMergeSort1000
  }

  def testMergeSort10 = {
    testSort(10, MergeSort.sortInt, "Merge sort")
  }

  def testMergeSort1000 = {
    testSort(1000, MergeSort.sortInt, "Merge sort")
  }

}
