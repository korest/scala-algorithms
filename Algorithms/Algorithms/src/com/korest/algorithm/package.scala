package com.korest

package object algorithm {

  val intComparator = (x: Int, y: Int) => { x > y }

  def swap[T](arr: Array[T], i: Int, j: Int) = {
    val el = arr(i)
    arr(i) = arr(j)
    arr(j) = el
  }

  def isArraySorted[T](arr: Array[T], comparator: (T, T) => Boolean): Boolean = {
    def isSorted(a: List[T]): Boolean = a match {
      case first :: second :: tail => if(comparator(first, second)) false else isSorted(tail)
      case _ => true
    }
    
    isSorted(arr.toList)
  }

  def isIntArraySorted(arr: Array[Int]): Boolean = {
    isArraySorted(arr, intComparator)
  }
  
}