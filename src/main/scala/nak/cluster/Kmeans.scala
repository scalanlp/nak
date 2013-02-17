/*
 Copyright 2013 Jason Baldridge

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

import org.apache.commons.logging.LogFactory
import org.apache.log4j.Logger
import org.apache.log4j.Level

import nak.util.CollectionUtil._

/**
 * A class for computing clusters for a set of points using k-means (specifically, Lloyd's algorithm).
 *
 * @param points	the set of points to be clustered
 * @param distance  the DistanceFunction to use to compute distance between pairs of points
 * @param maxChangeInDispersion each iteration of the algorithm produces a dispersion value, which is the squared sum distance from each centroid to the points it is responsible for. The minChangeInDispersion is a value that tells the algorithm to stop when change from one iteration to the next is less than this value.
 * @param maxIterations the maximum number of iterations to run k-means for
 *
 * @author jasonbaldridge
 */
class Kmeans(
  points: IndexedSeq[Point],
  distance: DistanceFunction,
  minChangeInDispersion: Double = 0.0001,
  maxIterations: Int = 100,
  fixedSeedForRandom: Boolean = false
) {

  private val LOG = LogFactory.getLog(Kmeans.getClass)

  private[this] val numDimensions = points.head.numDimensions
  private[this] val origin = Point(IndexedSeq.fill(numDimensions)(0.0))

  // Seed with 13 if consistency across runs is required.
  private[this] val random = 
    if (fixedSeedForRandom) new util.Random(13)
    else new util.Random(compat.Platform.currentTime)

  /**
   * Run the k-means algorithm on this set of points for some given k.
   *
   * @param k The number of clusters to produce.
   * @param restarts The number of times to run k-means from different random starting points.
   * @return A pair, the first element of which is the dispersion for the best set of centroids found, and the second element of which is that set of centroids.
   */
  def run(k: Int, restarts: Int = 25): (Double, IndexedSeq[Point]) = {
    val runResults = (1 to restarts).map { _ =>
      moveCentroids(chooseRandomCentroids(k))
    }

    val (bestDispersion, bestCentroids) = runResults.minBy(_._1)

    LOG.debug("Dispersion: " + bestDispersion)
    LOG.debug("Centroids: ")
    if (LOG.isDebugEnabled)
      bestCentroids.foreach(println)

    (bestDispersion, bestCentroids)
  }

  /**
   * Run the k-means algorithm starting from the given set of centroids.
   *
   * @return A pair, the first element of which is the dispersion for the
   * best set of centroids found, and the second element of which is that
   * set of centroids.
   */
  def moveCentroids(centroids: IndexedSeq[Point]): (Double, IndexedSeq[Point]) = {

    // Inner recursive function for computing next centroids
    def inner(centroids: IndexedSeq[Point],
      lastDispersion: Double,
      iteration: Int): (Double, IndexedSeq[Point]) = {

      LOG.debug("Iteration " + iteration)

      val (dispersion, memberships) = computeClusterMemberships(centroids)
      val updatedCentroids = computeCentroids(memberships)

      LOG.debug("Dispersion: " + dispersion)
      LOG.debug("Centroids: ")
      if (LOG.isDebugEnabled)
        updatedCentroids.foreach(println)

      val dispersionChange = lastDispersion - dispersion

      if (iteration > maxIterations || dispersionChange < minChangeInDispersion)
        (lastDispersion, centroids)
      else
        inner(updatedCentroids, dispersion, iteration + 1)
    }

    inner(centroids, Double.PositiveInfinity, 1)
  }

  /**
   *  Given a sequence of centroids, compute the cluster memberships for each point.
   *
   *  @param centroids A set of points representing centroids.
   *  @return A pair, the first element of which is the dispersion given these centroids, and the second of which is the list of centroid indices for each of the points being clustered (based on the nearest centroid to each).
   */
  def computeClusterMemberships(centroids: IndexedSeq[Point]) = {
    val (squaredDistances, memberships) = points.map { point =>
      val distances = centroids.map(distance(_, point))
      val shortestDistance = distances.min
      val closestCentroid = distances.indexWhere(shortestDistance==)
      (shortestDistance * shortestDistance, closestCentroid)
    }.unzip
    (squaredDistances.sum, memberships)
  }

  /**
   *  Given memberships for each point, compute the centroid for each cluster.
   */
  private[this] def computeCentroids(memberships: IndexedSeq[Int]) = {
    memberships.zip(points)
      .groupByKey
      .mapValues(group => group.reduce(_ ++ _) / group.length.toDouble)
      .toVector
      .sortBy(_._1)
      .map(_._2)
  }

  /**
   * Randomly choose k of the points as initial centroids.
   */
  private[this] def chooseRandomCentroids(k: Int) =
    random.shuffle(points).take(k)

}
