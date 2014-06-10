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

trait BoundedPriorityQueue[A] extends mutable.PriorityQueue[A] {
  def maxSize: Int

  override def +=(a: A): this.type = {
    if (size < maxSize) super.+=(a)
    else maybeReplaceLowest(a)
    this
  }

  override def ++=(xs: TraversableOnce[A]): this.type = {
    xs.foreach {this += _}
    this
  }

  override def +=(elem1: A, elem2: A, elems: A*): this.type = {
    this += elem1 += elem2 ++= elems
  }

  override def enqueue(elems: A*) {
    this ++= elems
  }

  private def maybeReplaceLowest(a: A) {
    // note: we use lt instead of gt here because the original
    // ordering used by this trait is reversed
    if (ord.lt(a, head)) {
      dequeue()
      super.+=(a)
    }
  }
}

object BoundedPriorityQueue {
  /**
   * Creates a new BoundedPriorityQueue instance.
   * @param maximum the max number of elements
   * @return a new bounded priority queue instance
   */
  def apply[A: Ordering](maximum: Int): BoundedPriorityQueue[A] = {
    // note: we reverse the ordering here because the mutable.PriorityQueue
    // class uses the highest element for its head/dequeue operations.
    val ordering = implicitly[Ordering[A]].reverse
    new mutable.PriorityQueue[A]()(ordering) with BoundedPriorityQueue[A] {
      implicit override val ord     = ordering
      override          val maxSize = maximum
    }
  }
}

