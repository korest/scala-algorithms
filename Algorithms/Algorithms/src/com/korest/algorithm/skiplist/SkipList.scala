package com.korest.algorithm.skiplist

import scala.util.Random

object SkipList {
  def main(args: Array[String]): Unit = {
    val n = 20
    val sl = new SkipList[Int, Int]
    for (i <- 1 to n) {
      val key = Random.nextInt(n)
      val value = Random.nextInt(n)
      sl.put(key, value)
    }
    sl.printSkipList()
  }
}

class SkipList[K <% Comparable[K], V] {

  case class SkipNode(var left: SkipNode = null, var right: SkipNode = null, var top: SkipNode = null, var bottom: SkipNode = null,
      var key: K, var value: V)
  
  var top: SkipNode = _
  var bottom: SkipNode = _

  def put(k: K, v: V): Boolean = {
    if(top == null) {
      top = SkipNode(key = k, value = v)
      bottom = top
      
      true
    } else {
      var current = top
      var added = false
      while(current != null && !added) {
        val compare = k.compareTo(current.key)
        if(compare == 0) {
          current = null
        } else if(compare > 0) {
          if(current.right != null) {
            current = current.right
          } else if(current.bottom != null) {
            current = current.bottom
          } else {
            /* before: left <------------> null
             *         left <--> node <--> null
             * after:  left <------------> node(new) 1/2 probability
             *         left <--> node <--> node(new)
             */
            current.right = SkipNode(left = current, key = k, value = v)
            promote(current.right)
            added = true
          }
        } else {
          if(current.left != null) {
            if(current.left.bottom != null) {
              current = current.left.bottom // go to left bottom
            } else {
              /* before: left <------------> right
               *         left <--> node <--> right
               * after:  left <--> node(new) 1/2 probability <------------> right
               *         left <--> node(new) <--------> node <------------> right
               */
              current.left.right = SkipNode(left = current.left, right = current, key = k, value = v)
              current.left = current.left.right
              promote(current.left)
              added = true
            }
          } else {
            /*
             * before: null <--------------> top <-----> ...
             *         null <--------------> bottom <--> ...
             * after:  node(new top) <-----> top <-----> ...
             *         node(new bottom) <--> bottom <--> ...
             */
            bottom.left = SkipNode(right = bottom, key = k, value = v)
            bottom = bottom.left
            top = bottom
            var rightTop = bottom.right.top
            while(rightTop != null) {
              top = SkipNode(right = rightTop, bottom = rightTop.bottom.left, key = k, value = v)
              rightTop.left = top
              rightTop.bottom.left.top = top
              rightTop = rightTop.top
            }
            
            added = true
          }
        }
      }
      
      added
    }
  }

  def get(k: K): Option[V] = {
    find(k) match {
      case Some(node) => Some(node.value)
      case None => None
    }
  }
  
  private def find(k: K): Option[SkipNode] = {
    if(top == null) {
      None
    } else {
      var result: Option[SkipNode] = None
      var current = top
      while(current != null && result.isEmpty) {
        val compare = k.compareTo(current.key)
        if(compare == 0) {
          result = Some(current)
        } else if(compare > 0) {
          if(current.right != null) {
            current = current.right
          } else if(current.bottom != null) {
            current = current.bottom
          } else {
            current = null
          }
        } else {
          if(current.left != null && current.left.bottom != null) {
            current = current.left.bottom
          } else {
            current = null
          }
        }
      }

      result
    }
  }
  
  def remove(k: K): Boolean = {
    find(k) match {
      case Some(n) => {
        var node = n
        while(node.bottom != null) {
          node = node.bottom
        }

        if(node.left != null) {
          while(node != null) {
            // before: left <--> node <--> right
            // after:  left <------------> right
            node.left.right = node.right
            if(node.right != null) {
              node.right.left = node.left
            }
            node = node.top
          }
        } else if(node.right != null) {
          /*
           * before:  null <--> node <----------------> right(2)
                      null <--> node <----------------> right(2)
                      null <--> node <--> right(1) <--> right(2)

           * after:   null <--> right(1) <--> right(2)
                      null <--> right(1) <--> right(2)
                      null <--> right(1) <--> right(2)
          */
          top = node.right
          bottom = top
          while(node != null) {
            if(node.right == null || node.right.key.compareTo(top.key) != 0) {
              val bottomRight = node.bottom.right
              bottomRight.top = SkipNode(left = node, right = node.right, bottom = bottomRight, key = bottomRight.key, value = bottomRight.value)
              if(node.right != null) {
                node.right.left = bottomRight.top
              }
              node.right =  bottomRight.top
            }

            top = node.right
            node = node.top
          }

          var newTop = top

          while(newTop != null) {
            if(newTop.left != null) {
              newTop.left.right = null
              newTop.left = null
            }

            newTop = newTop.bottom
          }

        } else {
          // before: null <--> node <--> null
          // after:  null
          top = null
          bottom = null
        }

        true
      }
      case None => false
    }
  }
  
  private def promote(n: SkipNode) = {
    var node = n
    while(Random.nextBoolean()) {
      node.top = SkipNode(bottom = node, key = node.key, value = node.value)
      var left = node.left
      while(left.left != null && left.top == null) {
        left = left.left
      }
      
      if(left.top == null) {
        /* before: null <------> node(new) <--> right
                   left <------> node(new) <--> right
         * after:  top(new) <--> node(new) <--> right
                   left <------> node(new) <--> right
         */

        left.top = SkipNode(bottom = left, right = node.top, key = left.key, value = left.value)
        node.top.left = left.top
        top = left.top
      } else {
        /* before: left <-----------------> right
                   left <--> node(new) <--> right
         * after:  left <--> node(new) <--> right
                   left <--> node(new) <--> right
         */
        val right = left.top.right
        left.top.right = node.top
        node.top.right = right
        node.top.left = left.top

        if(right != null) {
          right.left = node.top
        }
      }
      
      node = node.top
    }
  }
  
  def printSkipList() = {
    var current = top
    var rows = 0
    while(current != null) {
      rows += 1
      current = current.bottom
    }
    var cols = 0
    current = bottom
    while(current != null) {
      cols += 1
      current = current.right
    }
    val array = Array.ofDim[String](rows, cols)

    current = bottom
    for(i <- 0 to cols - 1) {
      var currentBottom = current
      for(j <- rows - 1 to 0 by -1) {
        if(currentBottom != null) {
          array(j)(i) = currentBottom.key.toString
          currentBottom = currentBottom.top
        } else {
          array(j)(i) = " "
        }
      }

      current = current.right
    }

    array.foreach(x => println(x.mkString("\t")))
  }
}