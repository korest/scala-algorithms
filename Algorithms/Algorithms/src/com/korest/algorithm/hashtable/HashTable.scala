package com.korest.algorithm.hashtable

import scala.reflect.ClassTag

object HashTable {
  val DEFAULT_SIZE = 16
  val LOAD_FACTOR = 0.75
}

class HashTable[K: ClassTag, V](var size: Int = HashTable.DEFAULT_SIZE) {

  case class Entry[K, V](key: K, var value: V, var next: Entry[K, V])

  var buckets: Array[Entry[K, V]] = new Array[Entry[K, V]](size)
  var count: Int = 0

  def put(key: K, value: V): Unit = {
    val position = indexForHash(hash(key))
    var entry = buckets(position)

    if (entry == null) {
      buckets(position) = Entry(key, value, null)
      count += 1
    } else {

      while (entry.next != null && !entry.key.equals(key)) {
        entry = entry.next
      }

      if (!entry.key.equals(key)) {
        entry.next = Entry(key, value, null)
        count += 1
      } else {
        entry.value = value
      }
    }

    if (count > size * HashTable.LOAD_FACTOR) {
      resize(size * 2)
    }
  }

  def get(key: K): V = {
    val position = indexForHash(hash(key))
    var entry = buckets(position)
    var result: V = null.asInstanceOf[V]

    if (entry != null) {
      while (entry != null && result == null) {
        if (entry.key.equals(key)) {
          result = entry.value
        } else {
          entry = entry.next
        }
      }
    }

    result
  }

  def remove(key: K): Boolean = {
    val position = indexForHash(hash(key))
    var entry = buckets(position)
    var result = false

    if (entry != null) {
      if (entry.key.equals(key)) {
        buckets(position) = entry.next
        count -= 1
        result = true
      } else {
        while (entry.next != null && !entry.next.key.equals(key)) {
          entry = entry.next
        }

        if (entry.next != null) {
          entry.next = entry.next.next
          count -= 1
          result = true
        }
      }
    }

    if (result && count < size / 4 && size / 4 >= HashTable.DEFAULT_SIZE) {
      resize(size / 4)
    }

    result
  }

  def resize(newSize: Int) = {
    size = newSize
    count = 0
    val bucks = buckets
    buckets = new Array[Entry[K, V]](size)
    for (buck <- bucks) {
      var entry = buck
      while (entry != null) {
        put(entry.key, entry.value)
        entry = entry.next
      }
    }
  }

  def hash(key: K): Int = {
    var h = key.hashCode()
    h ^= (h >>> 20) ^ (h >>> 12)
    h ^ (h >>> 7) ^ (h >>> 4)
  }

  def indexForHash(hash: Int): Int = {
    hash & (size - 1)
  }

  def print() = {
    for (buck <- buckets) {
      var entry = buck
      while (entry != null) {
        println("[%s, %s]".format(entry.key, entry.value))
        entry = entry.next
      }
    }
  }

  def keys: Array[K] = {
    val keys = new Array[K](count)
    var index = 0
    for (buck <- buckets) {
      var entry = buck
      while (entry != null) {
        keys(index) = entry.key
        index += 1
        entry = entry.next
      }
    }

    keys
  }
}
