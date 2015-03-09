package com.korest.algorithm.tree

class BinarySearchTree[T](comparator: (T, T) => Int) {

  case class Node[T](var left: Node[T], var right: Node[T], var value: T)

  var root: Node[T] = _

  def add(value: T): Unit = {
    if(root == null) {
      root = Node(null, null, value)
    } else {
      def addInternal(r: Node[T]): Unit = {
        val compare = comparator(value, r.value)
        if(compare > 0) {
          if(r.right == null) {
            r.right = Node(null, null, value)
          } else {
            addInternal(r.right)
          }
        } else if(compare < 0) {
          if(r.left == null) {
            r.left = Node(null, null, value)
          } else {
            addInternal(r.left)
          }
        }
      }

      addInternal(root)
    }
  }

  def find(value: T): Boolean = {
    def findInternal(r: Node[T]): Boolean = {
      if(r == null) {
        false
      } else {
        val compare = comparator(value, r.value)
        if (compare == 0) {
          true
        } else if (compare > 0) {
          findInternal(r.right)
        } else {
          findInternal(r.left)
        }
      }
    }

    findInternal(root)
  }

  def remove(value: T): Boolean = {
    def findPairInternal(parent: Node[T], child: Node[T]): (Node[T], Node[T]) = {
      if(child == null) {
        (null, null)
      } else {
        val compare = comparator(value, child.value)
        if (compare == 0) {
          (parent, child)
        } else if (compare > 0) {
          findPairInternal(child, child.right)
        } else {
          findPairInternal(child, child.left)
        }
      }
    }

    val nodes = findPairInternal(null, root)
    val parentNode = nodes._1
    val childNode = nodes._2

    if(childNode == null) {
      false
    } else {
      if(childNode.left != null && childNode.right != null) {
        var min = childNode.right
        var parent = childNode
        while (min.left != null) {
          parent = min
          min = min.left
        }

        val minVal = min.value
        min.value = childNode.value
        childNode.value = minVal

        parent.left = min.right
      } else if(childNode.left != null) {
        if(parentNode != null) {
          if(parentNode.left == childNode) {
            parentNode.left = childNode.left
          } else {
            parentNode.right = childNode.left
          }
        } else {
          root = childNode.left
        }
      } else if(childNode.right != null) {
        if(parentNode != null) {
          if(parentNode.left == childNode) {
            parentNode.left = childNode.right
          } else {
            parentNode.right = childNode.right
          }
        } else {
          root = childNode.right
        }
      } else {
        if(parentNode != null) {
          if (parentNode.left == childNode) {
            parentNode.left = null
          } else {
            parentNode.right = null
          }
        } else {
          root = null
        }
      }

      true
    }
  }

  def traverse(f: T => Unit) = {
    def traverseInternal(node: Node[T]): Unit = {
      if(node.left != null) {
        traverseInternal(node.left)
      }
      f(node.value)
      if(node.right != null) {
        traverseInternal(node.right)
      }
    }

    if(root != null) {
      traverseInternal(root)
    }
  }

}
