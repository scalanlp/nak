package nak.cluster

import scala.language.{ implicitConversions, postfixOps }

import breeze.numerics._
import breeze.linalg._
import breeze.util._
import breeze.util.Implicits._
import scala.collection.mutable.{ Set => MutableSet, ListBuffer }
import GDBSCAN._

/**
 * A clustering algorithm for density based clustering.
 *
 * @author Nepomuk Seiler
 * @param getNeighbours - determine the neighbours at the given point
 * @param isCorePoint - determine if this point is a core point based on its neighbourhood
 * @see http://en.wikipedia.org/wiki/DBSCAN
 * @see http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.71.1980
 */
class GDBSCAN[T](
  getNeighbours: (Point[T], Seq[Point[T]]) => Seq[Point[T]],
  isCorePoint: (Point[T], Seq[Point[T]]) => Boolean) {

  /**
   * Start clustering
   *
   * @param data - each row is treated as a feature vector
   * @return clusters - a list of clusters with
   */
  def cluster(data: DenseMatrix[T]): Seq[AbstractCluster[T]] = {
    // Visited - using row indices
    val visited = MutableSet[Point[T]]()
    val clustered = MutableSet[Point[T]]()

    // Init points
    val points = for (row <- 0 until data.rows) yield Point(row)(data(row, ::).inner)
    val noise = Noise[T]()

    // Start clustering
    points.collect {
      case point @ Point(row) if !(visited contains point) =>
        val neighbours = getNeighbours(point, points.filterNot(_.row == point.row))
        if (isCorePoint(point, neighbours)) {
          visited add point
          val cluster = Cluster[T](row)
          expand(point, neighbours, cluster)(points, visited, clustered)
          Some(cluster)
        } else {
          noise add point
          None // noise
        }
    }.flatten :+ noise
  }

  private def expand(point: Point[T], neighbours: Seq[Point[T]], cluster: AbstractCluster[T])(points: Seq[Point[T]], visited: MutableSet[Point[T]], clustered: MutableSet[Point[T]]) {
    cluster add point
    clustered add point
    neighbours.foldLeft(neighbours) {
      case (neighbourhood, neighbour @ Point(row)) =>
        // if not visited yet, create a new neighbourhood
        val newNeighbours = if (!(visited contains neighbour)) {
          visited add neighbour
          getNeighbours(neighbour, points.filterNot(_.row == neighbour.row))
        } else {
          Seq.empty
        }
        // Add to cluster if neighbour point isn't assigned to a  cluster yet
        if (!(clustered contains neighbour)) {
          cluster add neighbour
          clustered add neighbour
        }
        // if the neighbour point is a cluster, join the neighbourhood
        if (isCorePoint(neighbour, neighbourhood)) neighbourhood ++ newNeighbours else neighbourhood
    }

  }
}

object GDBSCAN {

  /** Holding the cluster point */
  case class Point[T](row: Int)(val value: DenseVector[T]) {
    override def toString() = s"[$row]: $value"
  }

  abstract class AbstractCluster[T] {
    val id: Long
    
    protected var _points = ListBuffer[Point[T]]()
    
    def add(p: Point[T]) {
      _points += p
    }
    
    def points: Seq[Point[T]] = Seq(_points: _*)
  }

  /** Cluster description */
  case class Cluster[T](id: Long) extends AbstractCluster[T] {
    override def toString() = s"Cluster [$id]\t:\t${_points.size} points\t${_points mkString "|"}"
  }

  case class Noise[T](id: Long = -1) extends AbstractCluster[T]

}

/**
 * Predefined functions for the original DBSCAN algorithm.
 * This can be used like this
 *
 * {{{
 *  val gdbscan = new GDBSCAN(
 *    DBSCAN.getNeighbours(epsilon = 1, distance = Kmeans.euclideanDistance),
 *    DBSCAN.isCorePoint(minPoints = 2)
 *  )
 * }}}
 */
object DBSCAN {

  /**
   * @param epsilon - minimum distance
   * @param distance - see [[nak.cluster.KMeans]] for distance functions
   */
  def getNeighbours(epsilon: Double, distance: (DenseVector[Double], DenseVector[Double]) => Double)(point: Point[Double], points: Seq[Point[Double]]): Seq[Point[Double]] = {
    points.filter(neighbour => distance(neighbour.value, point.value) < epsilon)
  }

  /**
   * @param minPoints - minimal number of points to be a core point
   */
  def isCorePoint(minPoints: Double)(point: Point[Double], neighbours: Seq[Point[Double]]): Boolean = {
    neighbours.size >= minPoints
  }
}
