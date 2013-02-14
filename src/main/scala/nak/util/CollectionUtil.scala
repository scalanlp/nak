package nak.util

import scala.collection.GenIterable
import scala.collection.GenIterableLike
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.IterableLike
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.Builder
import scala.util.Random
import scala.annotation.tailrec

/**
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
object CollectionUtil {

  //////////////////////////////////////////////////////
  // toTuple2: (T,T)
  // toTuple3: (T,T,T)
  // toTuple4: (T,T,T,T)
  // toTuple5: (T,T,T,T,T)
  //   - Convert this sequence to a tuple
  //////////////////////////////////////////////////////

  class Enriched_toTuple_Seq[A](seq: Seq[A]) {
    def toTuple2 = seq match { case Seq(a, b) => (a, b); case x => throw new AssertionError("Cannot convert sequence of length %s into Tuple2: %s".format(seq.size, x)) }
    def toTuple3 = seq match { case Seq(a, b, c) => (a, b, c); case x => throw new AssertionError("Cannot convert sequence of length %s into Tuple3: %s".format(seq.size, x)) }
    def toTuple4 = seq match { case Seq(a, b, c, d) => (a, b, c, d); case x => throw new AssertionError("Cannot convert sequence of length %s into Tuple4: %s".format(seq.size, x)) }
    def toTuple5 = seq match { case Seq(a, b, c, d, e) => (a, b, c, d, e); case x => throw new AssertionError("Cannot convert sequence of length %s into Tuple5: %s".format(seq.size, x)) }
  }
  implicit def enrich_toTuple_Seq[A](seq: Seq[A]): Enriched_toTuple_Seq[A] =
    new Enriched_toTuple_Seq(seq)

  class Enriched_toTuple_Array[A](seq: Array[A]) {
    def toTuple2 = seq match { case Array(a, b) => (a, b); case x => throw new AssertionError("Cannot convert array of length %s into Tuple2: %s".format(seq.size, x)) }
    def toTuple3 = seq match { case Array(a, b, c) => (a, b, c); case x => throw new AssertionError("Cannot convert array of length %s into Tuple3: %s".format(seq.size, x)) }
    def toTuple4 = seq match { case Array(a, b, c, d) => (a, b, c, d); case x => throw new AssertionError("Cannot convert array of length %s into Tuple4: %s".format(seq.size, x)) }
    def toTuple5 = seq match { case Array(a, b, c, d, e) => (a, b, c, d, e); case x => throw new AssertionError("Cannot convert array of length %s into Tuple5: %s".format(seq.size, x)) }
  }
  implicit def enrich_toTuple_Array[A](seq: Array[A]): Enriched_toTuple_Array[A] =
    new Enriched_toTuple_Array(seq)

  //////////////////////////////////////////////////////
  // +:(elem: B): Iterator[B]
  //   - Prepend an element to the iterator
  // :+(elem: B): Iterator[B]
  //   - Append an element to the end of the iterator
  //////////////////////////////////////////////////////

  class Enriched_prependAppend_Iterator[A](self: Iterator[A]) {
    /**
     * Prepend an item to the front of the iterator
     *
     * @param elem	the item to be prepended
     * @return a new iterator
     */
    def +:[B >: A](elem: B): Iterator[B] =
      Iterator(elem) ++ self

    /**
     * Append an item to the end of the iterator
     *
     * @param elem	the item to be appended
     * @return a new iterator
     */
    def :+[B >: A](elem: B): Iterator[B] =
      self ++ Iterator(elem)
  }
  implicit def enrich_prependAppend_Iterator[A](self: Iterator[A]): Enriched_prependAppend_Iterator[A] =
    new Enriched_prependAppend_Iterator(self)

  //////////////////////////////////////////////////////
  // groupByKey(): Map[T,Repr[U]]
  //   - For a collection of pairs (k,v), create a map from each `k` to the  
  //     collection of `v`s with which it is associated.
  //   - Equivalent to self.groupBy(_._1).map { case (k, elems) => (k, elems.map(_._2)) }
  //////////////////////////////////////////////////////

  class Enriched_groupByKey_Traversable[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]) {
    /**
     * For a collection of pairs (k,v), create a map from each `k` to the
     * collection of `v`s with which it is associated.
     *
     * Equivalent to self.groupBy(_._1).map { case (k, elems) => (k, elems.map(_._2)) }
     *
     * @return Map from `k`s to collections of `v`s
     */
    def groupByKey[K, V, That](implicit ev: A <:< (K, V), bf: CanBuildFrom[Repr, V, That]): Map[K, That] = {
      val m = mutable.Map.empty[K, Builder[V, That]]
      for ((key, value) <- self.map(ev)) {
        val bldr = m.getOrElseUpdate(key, bf(self.asInstanceOf[Repr]))
        bldr += value
      }
      val b = immutable.Map.newBuilder[K, That]
      for ((k, v) <- m)
        b += ((k, v.result))
      b.result
    }
  }
  implicit def enrich_groupByKey_Traversable[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]): Enriched_groupByKey_Traversable[A, Repr] =
    new Enriched_groupByKey_Traversable(self)

  //////////////////////////////////////////////////////
  // ungroup(): Iterator[(A, B)]
  //   - For a map with collections for values, return an iterator of pairs
  //     where each key is paired with each item in its value collection
  //   - Equivalent to self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
  //////////////////////////////////////////////////////

  class Enriched_ungroup_GenTraversableOnce[A, B](self: GenTraversableOnce[(A, GenTraversableOnce[B])]) {
    /**
     * For a map with collections for values, return an iterator of pairs
     * where each key is paired with each item in its value collection.
     *
     * Equivalent to self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
     *
     * @return an iterator of pairs
     */
    def ungroup() = self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
  }
  implicit def enrich_ungroup_GenTraversableOnce[A, B](self: GenTraversableOnce[(A, GenTraversableOnce[B])]): Enriched_ungroup_GenTraversableOnce[A, B] =
    new Enriched_ungroup_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // dropRightWhile(p: A => Boolean): Repr
  //////////////////////////////////////////////////////

  class Enriched_dropRightWhile_Iterable[A, Repr <: Iterable[A]](self: IterableLike[A, Repr]) {
    def dropRightWhile[That](p: A => Boolean)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      val buffer = mutable.Buffer[A]()
      for (x <- self) {
        buffer += x
        if (!p(x)) {
          b ++= buffer
          buffer.clear()
        }
      }
      b.result
    }
  }
  implicit def enrich_dropRightWhile_Iterable[A, Repr <: Iterable[A]](self: IterableLike[A, Repr]): Enriched_dropRightWhile_Iterable[A, Repr] =
    new Enriched_dropRightWhile_Iterable(self)

  class Enriched_dropRightWhile_String(self: String) {
    def dropRightWhile(p: Char => Boolean): String = {
      val b = stringCanBuildFrom()
      val buffer = mutable.Buffer[Char]()
      for (x <- self) {
        buffer += x
        if (!p(x)) {
          b ++= buffer
          buffer.clear()
        }
      }
      b.result
    }
  }
  implicit def enrich_dropRightWhile_String(self: String): Enriched_dropRightWhile_String =
    new Enriched_dropRightWhile_String(self)

  //////////////////////////////////////////////////////
  // splitAt(n: Int) 
  //   - Split this collection at the specified index
  //   - Useful since Iterator.take doesn't guarantee the state of the original Iterator
  //   - Extend Traversable.splitAt to Iterator
  //////////////////////////////////////////////////////

  class Enriched_splitAt_Iterator[A](self: Iterator[A]) {
    /**
     * Safely split this iterator at the specified index.  The 'first'
     * iterator must be exhausted completely before the items in the 'second'
     * iterator can be accessed.
     *
     * Inspired by Traversable.splitAt
     *
     * @param n	The index at which to split the collection
     * @return	a pair: the items before the split point and the items
     *          starting with the split point
     */
    def splitAt(n: Int): (Iterator[A], Iterator[A]) = {
      var i = 0

      val first: Iterator[A] =
        new Iterator[A] {
          def next(): A = {
            assert(hasNext, "first has already been read completely")
            i += 1; self.next
          }
          def hasNext() = i < n && self.hasNext
        }

      val second: Iterator[A] =
        new Iterator[A] {
          def next(): A = {
            assert(i >= n, "first has NOT YET been read completely")
            assert(hasNext, "second has already been read completely")
            i += 1; self.next
          }
          def hasNext() = self.hasNext
        }

      (first, second)
    }
  }
  implicit def enrich_splitAt_Iterator[A](self: Iterator[A]): Enriched_splitAt_Iterator[A] =
    new Enriched_splitAt_Iterator(self)

  //////////////////////////////////////////////////////
  // split(delim: A): Iterator[Repr[A]]
  //   - Split this collection on each occurrence of the delimiter 
  //   - Inspired by String.split
  //////////////////////////////////////////////////////

  class Enriched_split_Iterator[A](self: Iterator[A]) {
    /**
     * Split this collection on each occurrence of the delimiter.  Delimiters
     * do not appear in the output.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split(delim: A): Iterator[Vector[A]] =
      split(delim, Vector.newBuilder[A])

    /**
     * Split this collection on each occurrence of the delimiter.  Delimiters
     * do not appear in the output.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A, builder: => Builder[A, That]): Iterator[That] =
      self.splitWhere(_ == delim, builder)
  }
  implicit def enrich_split_Iterator[A](self: Iterator[A]): Enriched_split_Iterator[A] =
    new Enriched_split_Iterator(self)

  class Enriched_split_GenTraversable[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * Split this collection on each occurrence of the delimiter.  Delimiters
     * do not appear in the output.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A)(implicit bf: CanBuildFrom[Repr, A, That]): Iterator[That] =
      self.toIterator.split(delim, bf(self.asInstanceOf[Repr]))
  }
  implicit def enrich_split_GenTraversable[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]): Enriched_split_GenTraversable[A, Repr] =
    new Enriched_split_GenTraversable(self)

  //////////////////////////////////////////////////////
  // splitWhere(p: A => Boolean): Iterator[Repr[A]]
  //   - Split this on items for which the predicate is true 
  //////////////////////////////////////////////////////

  class Enriched_splitWhere_Iterator[A](self: Iterator[A]) {
    /**
     * Split this on items for which the predicate is true.  Delimiters
     * do not appear in the output.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere(p: A => Boolean): Iterator[Vector[A]] =
      splitWhere(p, Vector.newBuilder[A])

    /**
     * Split this on items for which the predicate is true.  Delimiters
     * do not appear in the output.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean, builder: => Builder[A, That]): Iterator[That] =
      new Iterator[That] {
        var buffer = mutable.Queue[That]()
        def next(): That = {
          assert(this.hasNext, "next on empty iterator")
          buffer.dequeue
        }

        def hasNext() = {
          if (buffer.isEmpty && self.hasNext) {
            takeUntilNext(builder)
          }
          buffer.nonEmpty
        }

        @tailrec
        private def takeUntilNext(builder: => Builder[A, That]): Unit = {
          val (empty, item) = takeUntilDelim(builder)
          buffer.enqueue(item)
          if (empty)
            if (self.hasNext)
              takeUntilNext(builder)
            else
              buffer.clear()
        }

        @tailrec
        private def takeUntilDelim(b: Builder[A, That], empty: Boolean = true): (Boolean, That) = {
          if (self.hasNext) {
            val x = self.next
            if (p(x))
              (empty, b.result)
            else
              takeUntilDelim(b += x, false)
          }
          else
            (empty, b.result)
        }
      }
  }
  implicit def enrich_splitWhere_Iterator[A](self: Iterator[A]): Enriched_splitWhere_Iterator[A] =
    new Enriched_splitWhere_Iterator(self)

  class Enriched_splitWhere_GenTraversable[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * Split this on items for which the predicate is true.  Delimiters
     * do not appear in the output.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean)(implicit bf: CanBuildFrom[Repr, A, That]): Iterator[That] =
      self.toIterator.splitWhere(p, bf(self.asInstanceOf[Repr]))
  }
  implicit def enrich_splitWhere_GenTraversable[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]): Enriched_splitWhere_GenTraversable[A, Repr] =
    new Enriched_splitWhere_GenTraversable(self)

  //////////////////////////////////////////////////////
  // zipSafe(that: GenTraversable[B]): Repr[(A,B)]
  //   - zip this collection with another, throwing an exception if they are
  //     not of equal length.
  //////////////////////////////////////////////////////

  class Enriched_zipSafe_Iterator[A](self: Iterator[A]) {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[B](that: GenTraversableOnce[B]) = {
      val thatItr = that.toIterator
      new Iterator[(A, B)] {
        def hasNext = {
          val hn = self.hasNext
          assert(hn == thatItr.hasNext, "Attempting to zipSafe collections of different lengths.")
          hn
        }
        def next() = {
          assert(self.hasNext == thatItr.hasNext, "Attempting to zipSafe collections of different lengths.")
          (self.next, thatItr.next)
        }
      }
    }
  }
  implicit def enrich_zipSafe_Iterator[A](self: Iterator[A]): Enriched_zipSafe_Iterator[A] =
    new Enriched_zipSafe_Iterator(self)

  class Enriched_zipSafe_GenTraversable[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[A1 >: A, B, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      b ++= (self.toIterator zipSafe that)
      b.result
    }
  }
  implicit def enrich_zipSafe_GenTraversable[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]): Enriched_zipSafe_GenTraversable[A, Repr] =
    new Enriched_zipSafe_GenTraversable(self)

  class Enriched_zipSafe_Tuple_of_Iterator[A, B](self: (Iterator[A], GenTraversableOnce[B])) {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe = self._1 zipSafe self._2
  }
  implicit def enrich_zipSafe_Tuple_of_Iterator[A, B](self: (Iterator[A], GenTraversableOnce[B])): Enriched_zipSafe_Tuple_of_Iterator[A, B] =
    new Enriched_zipSafe_Tuple_of_Iterator(self)

  class Enriched_zipSafe_Tuple_of_GenTraversable[A, Repr <: GenTraversable[A], B](self: (GenTraversableLike[A, Repr], GenTraversableOnce[B])) {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[A1 >: A, That](implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(self._1.asInstanceOf[Repr])
      b ++= (self._1.toIterator zipSafe self._2)
      b.result
    }
  }
  implicit def enrich_zipSafe_Tuple_of_GenTraversable[A, Repr <: GenTraversable[A], B](self: (GenTraversableLike[A, Repr], GenTraversableOnce[B])): Enriched_zipSafe_Tuple_of_GenTraversable[A, Repr, B] =
    new Enriched_zipSafe_Tuple_of_GenTraversable(self)

  //////////////////////////////////////////////////////
  // unzip(): (Iterator[A], Iterator[B])
  //   - Extend unzip functionality to Iterator
  //////////////////////////////////////////////////////

  class Enriched_unzip_Iterator[T, U](self: Iterator[(T, U)]) {
    def unzip(): (Vector[T], Vector[U]) =
      this.unzip(Vector.newBuilder[T], Vector.newBuilder[U])

    def unzip[ThatT <: Iterable[T], ThatU <: Iterable[U]](tBuilder: => Builder[T, ThatT], uBuilder: => Builder[U, ThatU]): (ThatT, ThatU) = {
      val tBldr = tBuilder
      val uBldr = uBuilder
      for ((t, u) <- self) {
        tBldr += t
        uBldr += u
      }
      (tBldr.result, uBldr.result)
    }
  }
  implicit def enrich_unzip_Iterator[T, U](self: Iterator[(T, U)]) = new Enriched_unzip_Iterator(self)

  //////////////////////////////////////////////////////
  // mapKeys(f: T => R): Repr[(R,U)]
  //   - In a collection of pairs, map a function over the first item of each pair.
  //   - Functionally equivalent to:
  //         this.map{case (k,v) => f(k) -> v}
  //////////////////////////////////////////////////////

  class Enriched_mapKeys_GenTraversable[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) {
    /**
     * In a collection of pairs, map a function over the first item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f	function to map over the first item of each pair
     * @return a collection of pairs
     */
    def mapKeys[R, That](f: T => R)(implicit bf: CanBuildFrom[Repr, (R, U), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for ((k, v) <- self) b += f(k) -> v
      b.result
    }
  }
  implicit def enrich_mapKeys_GenTraversable[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]): Enriched_mapKeys_GenTraversable[T, U, Repr] =
    new Enriched_mapKeys_GenTraversable(self)

  class Enriched_mapKeys_Iterator[T, U](self: Iterator[(T, U)]) {
    /**
     * In a collection of pairs, map a function over the first item of each
     * pair.
     *
     * @param f	function to map over the first item of each pair
     * @return a collection of pairs
     */
    def mapKeys[R](f: T => R) = new Iterator[(R, U)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, v) = self.next()
        f(k) -> v
      }
    }
  }
  implicit def enrich_mapKeys_Iterator[T, U](self: Iterator[(T, U)]): Enriched_mapKeys_Iterator[T, U] =
    new Enriched_mapKeys_Iterator(self)

  //////////////////////////////////////////////////////
  // mapVals(f: U => R): Repr[(T,R)]
  //   - In a collection of pairs, map a function over the second item of each pair.
  //   - Ensures that the map is computed at call-time, and not returned as a view as `Map.mapValues` would do.
  //   - Equivalent to: this.map { case (k,v) => k -> f(v) }
  //////////////////////////////////////////////////////

  class Enriched_mapVals_GenTraversable[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapVals[R, That](f: U => R)(implicit bf: CanBuildFrom[Repr, (T, R), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for ((k, v) <- self) b += k -> f(v)
      b.result
    }
  }
  implicit def enrich_mapVals_GenTraversable[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]): Enriched_mapVals_GenTraversable[T, U, Repr] =
    new Enriched_mapVals_GenTraversable(self)

  class Enriched_mapVals_Iterator[T, U](self: Iterator[(T, U)]) {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapVals[R](f: U => R) = new Iterator[(T, R)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, v) = self.next()
        k -> f(v)
      }
    }
  }
  implicit def enrich_mapVals_Iterator[T, U](self: Iterator[(T, U)]): Enriched_mapVals_Iterator[T, U] =
    new Enriched_mapVals_Iterator(self)

  //////////////////////////////////////////////////////
  // avg(): A
  //   - Find the average (mean) of this collection of numbers
  //////////////////////////////////////////////////////

  class Enrich_avg_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg(implicit num: Fractional[A]) = {
      val (total, count) = self.toIterator.foldLeft((num.zero, num.zero)) {
        case ((total, count), x) => (num.plus(total, x), num.plus(count, num.one))
      }
      num.div(total, count)
    }
  }
  implicit def enrich_avg_GenTraversableOnce[A](self: GenTraversableOnce[A]): Enrich_avg_GenTraversableOnce[A] =
    new Enrich_avg_GenTraversableOnce(self)

  class Enrich_avg_Int_GenTraversableOnce(self: GenTraversableOnce[Int]) {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg = {
      val (total, count) = self.toIterator.foldLeft((0, 0)) {
        case ((total, count), x) => (total + x, count + 1)
      }
      total.toDouble / count
    }
  }
  implicit def enrich_avg_Int_GenTraversableOnce(self: GenTraversableOnce[Int]): Enrich_avg_Int_GenTraversableOnce =
    new Enrich_avg_Int_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // normalize(): Repr[A]
  //   - Normalize this collection of numbers by dividing each by the sum
  //////////////////////////////////////////////////////

  class Enriched_normalize_GenTraversable[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * Normalize this collection of numbers by dividing each by the sum
     *
     * @return normalized values
     */
    def normalize[That](implicit num: Fractional[A], bf: CanBuildFrom[Repr, A, That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.sum
      for (x <- self) b += num.div(x, total)
      b.result
    }
  }
  implicit def enrich_normalize_GenTraversable[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]): Enriched_normalize_GenTraversable[A, Repr] =
    new Enriched_normalize_GenTraversable(self)

  class Enriched_normalize_Int_GenTraversable[Repr <: GenTraversable[Int]](self: GenTraversableLike[Int, Repr]) {
    /**
     * Normalize this collection of numbers by dividing each by the sum
     *
     * @return normalized values
     */
    def normalize[That](implicit bf: CanBuildFrom[Repr, Double, That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.sum.toDouble
      for (x <- self) b += x / total
      b.result
    }
  }
  implicit def enrich_normalize_Int_GenTraversable[Repr <: GenTraversable[Int]](self: GenTraversableLike[Int, Repr]): Enriched_normalize_Int_GenTraversable[Repr] =
    new Enriched_normalize_Int_GenTraversable(self)

  //////////////////////////////////////////////////////
  // normalizeValues(): Repr[(T,U)]
  //   - Normalize this values in this collection of pairs
  //////////////////////////////////////////////////////

  class Enriched_normalizeValues_GenTraversable[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) {
    /**
     * Normalize this values in this collection of pairs
     *
     * @return a collection of pairs
     */
    def normalizeValues[That](implicit num: Fractional[U], bf: CanBuildFrom[Repr, (T, U), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.foldLeft(num.zero)((z, a) => num.plus(z, a._2))
      for ((k, v) <- self) b += k -> num.div(v, total)
      b.result
    }
  }
  implicit def enrich_normalizeValues_GenTraversable[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]): Enriched_normalizeValues_GenTraversable[T, U, Repr] =
    new Enriched_normalizeValues_GenTraversable(self)

  class Enriched_normalizeValues_Int_GenTraversable[T, Repr <: GenTraversable[(T, Int)]](self: GenTraversableLike[(T, Int), Repr]) {
    /**
     * Normalize this values in this collection of pairs
     *
     * @return a collection of pairs
     */
    def normalizeValues[That](implicit bf: CanBuildFrom[Repr, (T, Double), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.foldLeft(0)((z, a) => z + a._2).toDouble
      for ((k, v) <- self) b += k -> (v / total)
      b.result
    }
  }
  implicit def enrich_normalizeValues_Int_GenTraversable[T, Repr <: GenTraversable[(T, Int)]](self: GenTraversableLike[(T, Int), Repr]): Enriched_normalizeValues_Int_GenTraversable[T, Repr] =
    new Enriched_normalizeValues_Int_GenTraversable(self)

  //////////////////////////////////////////////////////
  // Conversion (.toX) methods
  //////////////////////////////////////////////////////
  class Enriched_toVector_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    def toVector =
      if (self.isEmpty) Vector.empty[A]
      else Vector.newBuilder[A] ++= self.toIterator result
  }
  implicit def enrich_toVector_GenTraversableOnce[A](self: GenTraversableOnce[A]): Enriched_toVector_GenTraversableOnce[A] =
    new Enriched_toVector_GenTraversableOnce(self)
  implicit def addToVectorToArray[A](self: Array[A]): Enriched_toVector_GenTraversableOnce[A] =
    new Enriched_toVector_GenTraversableOnce(self)

}
