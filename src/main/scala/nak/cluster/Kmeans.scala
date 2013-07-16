/*
 Copyright 2013 ScalaNLP

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
package nak.cluster

import breeze.math.{MutableInnerProductSpace, MutableNormedSpace}
import breeze.numerics._
import breeze.linalg._
import breeze.util._
import breeze.util.Implicits._
import collection.mutable.ArrayBuffer
import breeze.stats.distributions.Multinomial
import com.typesafe.scalalogging.log4j.Logging

import nak.util.CollectionUtil._

/**
  * A class for computing clusters for a set of points using k-means (specifically,
  *  Lloyd's algorithm).
  *
  * @param points	the set of points to be clustered
  * @param maxChangeInDispersion each iteration of the algorithm produces a dispersion
  *    value, which is the squared sum distance from each centroid to the points it is
  *    responsible for. The minChangeInDispersion is a value that tells the algorithm to
  *    stop when change from one iteration to the next is less than this value.
  * @param maxIterations the maximum number of iterations to run k-means for
  *
 */
class Kmeans[T](
  points: IndexedSeq[T],
  distanceFun: (T,T)=>Double = Kmeans.euclideanDistance,
  minChangeInDispersion: Double = 0.0001,
  maxIterations: Int = 100,
  fixedSeedForRandom: Boolean = false
)(implicit space: MutableInnerProductSpace[T, Double]) extends Logging {
  import space._
  
  // Seed with 13 if consistency across runs is required.
  private[this] val random = 
    if (fixedSeedForRandom) new util.Random(13)
    else new util.Random(compat.Platform.currentTime)

  /**
    * Run the k-means algorithm on this set of points for some given k.
    *
    * @param k The number of clusters to produce.
    * @param restarts The number of times to run k-means from different random
    *     starting points.
    * 
    * @return A pair, the first element of which is the dispersion for the best
    *     set of centroids found, and the second element of which is that set of
    *     centroids.
    */
  def run(k: Int, restarts: Int = 25): (Double, IndexedSeq[T]) = {
    val runResults = (1 to restarts).map(_ => moveCentroids(chooseRandomCentroids(k)))
    val (bestDispersion, bestCentroids) = runResults.minBy(_._1)
    logger.debug("Dispersion: " + bestDispersion)
    (bestDispersion, bestCentroids)
  }

  /**
    * Run the k-means algorithm starting from the given set of centroids. This
    * is an iterative version since it runs faster than a nicer recursive one.
    *
    * @return A pair, the first element of which is the dispersion for the
    *     best set of centroids found, and the second element of which is that
    *     set of centroids.
    */
  private[this] def moveCentroids(centroids: IndexedSeq[T]): (Double, IndexedSeq[T]) = {
    val numClusters = centroids.length
    var iteration = 0
    var lastDispersion = Double.PositiveInfinity
    var dispersionChange = Double.PositiveInfinity
    var changingCentroids = centroids
    logger.debug("Starting k-means")
    while (iteration < maxIterations && dispersionChange > minChangeInDispersion) {
      val (dispersion, memberships) = computeClusterMemberships(changingCentroids)
      changingCentroids = computeCentroids(memberships,numClusters)
      dispersionChange = math.abs(lastDispersion - dispersion)
      lastDispersion = dispersion
      logger.debug(s"Iteration  $iteration $lastDispersion $dispersionChange")
      iteration += 1
    }
    (lastDispersion, changingCentroids)
  }

  /**
    *  Given a sequence of centroids, compute the cluster memberships for each point.
    *
    *  @param centroids A sequence of points representing centroids.
    *  @return A pair, the first element of which is the dispersion given these centroids,
    *       and the second of which is the list of centroid indices for each of the points
    *       being clustered (based on the nearest centroid to each).
    */
  def computeClusterMemberships(centroids: IndexedSeq[T]) = {
    val (squaredDistances, memberships) = points.par.map { point =>
      val distances = centroids.map(c=>distanceFun(c,point))
      val shortestDistance = distances.min
      val closestCentroid = distances.indexWhere(shortestDistance==)
      assert(closestCentroid > -1)
      (shortestDistance * shortestDistance, closestCentroid)
    }.toIndexedSeq.unzip
    (squaredDistances.sum, memberships)
  }

  /**
    * Given memberships for each point, compute the centroid for each cluster.
    */
  private[this] def computeCentroids(memberships: IndexedSeq[Int], numClusters: Int) = {
    val centroids = IndexedSeq.fill(numClusters)(zeros(points.head))
    val counts = Array.fill(numClusters)(0)
    var index = 0
    while (index < points.length) {
      val clusterId = memberships(index)
      if (clusterId > -1) {
        centroids(clusterId) += points(index)
        counts(clusterId) += 1
      }
      index += 1
    }
    (for ((centroid, count) <- centroids.zip(counts).par) yield {
      centroid / count.toDouble
    }).toIndexedSeq

  }

  /**
    * Randomly choose k of the points as initial centroids.
    */
  private[this] def chooseRandomCentroids(k: Int) =
    random.shuffle(points).take(k)

}

/**
  * A companion to hold distance functions.
  */
object Kmeans {

  import breeze.linalg._

  /**
    * Compute cosine distance: 1-cosine(a,b)
    */ 
  val cosineDistance = (a: Vector[Double], b: Vector[Double]) => {
    1 - (a dot b)/(norm(a)*norm(b))
  }
  
  /**
    * Compute Manhattan distance (l1 norm).
    */ 
  val manhattanDistance = (a: Vector[Double], b: Vector[Double]) => {
    (a-b).norm(1)
  }

  /**
    * Compute euclidean distance (l2 norm).
    */ 
  val euclideanDistance = (a: Vector[Double], b: Vector[Double]) => {
    norm(a-b)
  }

}
