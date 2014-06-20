package nak.util

import scala.collection.mutable

class GrowableIndex[T] extends (T => Int) {

  import collection.mutable

  var nextIndex = 0
  val mapping   = mutable.HashMap[T, Int]()

  def apply(element: T): Int = {
    if (mapping.isDefinedAt(element))
      mapping(element)
    else {
      val idx = nextIndex
      mapping(element) = idx
      nextIndex += 1
      idx
    }
  }

  def toMap = mapping.toMap

  def size = nextIndex
}
