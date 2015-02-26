package com.korest.algorithm.insertionsort

import com.korest.algorithm._

object InsertionSort {
  
  def sort[T](arr: Array[T], comparator: (T, T) => Boolean) = {
    for(i <- 1 to arr.length - 1) {
      var j = i
      while(j > 0 && comparator(arr(j - 1), arr(j))) {
        swap(arr, j, j - 1)
        j = j - 1
      }
    }
  }
  
  def sortInt(arr: Array[Int]) {
    sort(arr, intComparator)
  }
  
  def sortBinary[T](arr: Array[T], comparator: (T, T) => Boolean) = {
    for(i <- 1 to arr.length - 1) {
      var left = 0
      var right = i
      while(left < right) {
        var middle = (left + right) / 2
        if(comparator(arr(i), arr(middle))) {
          left = middle + 1
        } else {
          right = middle
        }
      }
      
      for(j <- i to left + 1 by -1) {
        swap(arr, j - 1, j)
      }
    }
  }
  
  def sortBinaryInt(arr: Array[Int]) = {
    sortBinary(arr, intComparator)
  }
  
}