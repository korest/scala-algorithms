package com.korest.algorithm.quicksort

import com.korest.algorithm._

object QuickSort {

  def quickSort[T](arr: Array[T], comparator: (T, T) => Boolean) = {
    def partition[T](p: Int, q: Int): Int = {
      val pivot = arr(q)
      var i = p - 1
      for(j <- p to q - 1) {
        if(comparator(pivot, arr(j))) {
          i = i + 1
          swap(arr, i, j)
        }
      }
      i = i + 1
      swap(arr, i, q)
      
      i
    }
  
    def sort[T](arr: Array[T], p: Int, q: Int): Unit = {
      if (p < q) {
        val i = partition(p, q)
        sort(arr, p, i - 1)
        sort(arr, i + 1, q)
      }
    }
    
    sort(arr, 0, arr.length - 1)
  }
  
  def quickSortInt(arr: Array[Int]) = {
    quickSort(arr, intComparator)
  }
}