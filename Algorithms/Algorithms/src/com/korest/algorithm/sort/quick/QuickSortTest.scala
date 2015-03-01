package com.korest.algorithm.sort.quick

import com.korest.algorithm.sort.BaseSortTest

object QuickSortTest extends BaseSortTest {

  def main(args: Array[String]): Unit = {
    testQuickSort10
    testQuickSort1000
  }
  
  def testQuickSort10 = {
    testSort(10, QuickSort.sortInt, "Quick sort")
  }
  
  def testQuickSort1000 = {
    testSort(1000, QuickSort.sortInt, "Quick sort")
  }
}