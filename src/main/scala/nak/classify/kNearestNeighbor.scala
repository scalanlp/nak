/*
 * dialogue -- The University of Washington Dialogue Framework
 *
 * Copyright 2013 - Gabriel Schubiner
 *
 * kNearestNeighbor.scala is part of dialogue.
 *
 * dialogue is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * dialogue is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with dialogue.  If not, see <http://www.gnu.org/licenses/>.
 */

package nak.classify

import nak.data.Example
import breeze.linalg.Counter
import breeze.generic.UFunc.UImpl2
import scala.collection.mutable
import nak.util.BoundedPriorityQueue
import breeze.storage.DefaultArrayValue
import nak.space.dm.euclidean
import breeze.linalg._

/**
 * dialogue
 * 6/8/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
//(implicit dm: UImpl2[L, T, T, Double])
class kNearestNeighbor[L, T,D](c: Iterable[Example[L, T]],
                             k: Int = 1)(implicit dm: UImpl2[D, T, T, Double]) extends Classifier[L, T] {
  /** For the observation, return the score for each label that has a nonzero
    * score.
    *
    * TODO: Think about how to implement this well (BPQ implementation)
    * TODO: Think about how scores should be represented. (Currently overloads assumed
    *       probability distribution return value as distances, but could possibly
    *       formulate as prob distribution over examples as seen in NCA paper
    *       or make up some probability distribution over the labels contained in the
    *       nearest neighbor set, where prob is inversely prop. to distance.to nearest label.
    */
  override def scores(o: T): Counter[L, Double] = {
    val pq = BoundedPriorityQueue[(L, Double)](k)(Ordering.by((_: (_, Double))._2))
    pq ++ c.map(e => (e.label, dm(e.features, o)))
    implicit val default = DefaultArrayValue[Double](Double.PositiveInfinity)
    pq.foldLeft(Counter[L, Double]())((minAcc: Counter[L, Double], next: (L, Double)) =>
    if (next._2 < minAcc(next._1)) {
      minAcc.update(next._1, next._2)
      minAcc
    } else minAcc)
  }
}

object kNearestNeighbor {

  class Trainer[L, T, D](k: Int = 1)(implicit dm: UImpl2[D, T, T, Double]) extends Classifier.Trainer[L, T] {
    type MyClassifier = kNearestNeighbor[L, T,D]

    override def train(data: Iterable[Example[L, T]]): MyClassifier = new kNearestNeighbor[L,T,D](data, k)
  }

}