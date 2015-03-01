package com.korest.algorithm.sort.insertion

import com.korest.algorithm.sort.BaseSortTest

object InsertionSortTest extends BaseSortTest {

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
  
}