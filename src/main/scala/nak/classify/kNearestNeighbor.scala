package nak.classify

import nak.data.Example
import breeze.generic.UFunc.UImpl2
import scala.collection.mutable
import breeze.storage.DefaultArrayValue
import breeze.linalg._
import breeze.collection.mutable.Beam

/**
 * kNearestNeighbor
 * 6/8/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
class kNearestNeighbor[L, T, D](c: Iterable[Example[L, T]],
                                k: Int = 1)(implicit dm: UImpl2[D, T, T, Double]) extends Classifier[L, T] {


  // Iterable of (example, distance) tuples
  type DistanceResult = Iterable[(Example[L,T],Double)]

  /*
   * Additional method to extract distances of k nearest neighbors
   */
  def distances(o: T): DistanceResult = {
    val beam = Beam[(Example[L,T], Double)](k)(Ordering.by(-(_: (_, Double))._2))
    beam ++= c.map(e => (e, dm(e.features, o)))
  }

  /** For the observation, return the max voting label with prob = 1.0
    */
  override def scores(o: T): Counter[L, Double] = {
    // Beam reverses ordering from min heap to max heap, but we want min heap
    // since we are tracking distances, not scores.
    val beam = Beam[(L, Double)](k)(Ordering.by(-(_: (_, Double))._2))

    // Add all examples to beam, tracking label and distance from testing point
    beam ++= c.map(e => (e.label, dm(e.features, o)))

    // Max voting classification rule
    val predicted = beam.groupBy(_._1).maxBy(_._2.size)._1

    // Degenerate discrete distribution with prob = 1.0 at predicted label
    Counter((predicted, 1.0))
  }
}

object kNearestNeighbor {

  class Trainer[L, T, D](k: Int = 1)(implicit dm: UImpl2[D, T, T, Double]) extends Classifier.Trainer[L, T] {
    type MyClassifier = kNearestNeighbor[L, T, D]

    override def train(data: Iterable[Example[L, T]]): MyClassifier = new kNearestNeighbor[L, T, D](data, k)
  }

}
