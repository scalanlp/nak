package nak.cluster

import org.scalatest._
import breeze.linalg._
import breeze.numerics._
import nak.cluster.GDBSCAN._

/**
 * Testing GDBSCAN clustering algorithm
 */
class GDBSCANTest extends FlatSpec with Matchers {

  "GDBSCAN" should "work as DBSCAN" in {
    val gdbscan = new GDBSCAN(
      DBSCAN.getNeighbours(epsilon = 1, distance = Kmeans.euclideanDistance),
      DBSCAN.isCorePoint(minPoints = 2)
    )
    val input = DenseMatrix(
      (0.9, 1.0),
      (1.0, 1.0),
      (1.0, 1.1),
      (5.0, 5.0), // NOISE

      (15.0, 15.0),
      (15.0, 14.1),
      (15.3, 15.0)
    )
    val cluster = gdbscan cluster input
    val clusterPoints = cluster.map(_.points.map(_.value.toArray))

    cluster.size shouldBe 2
    clusterPoints(0) should contain only (Array(0.9, 1.0), Array(1.0, 1.0), Array(1.0, 1.1))
    clusterPoints(1) should contain only (Array(15.0, 15.0), Array(15.0, 14.1), Array(15.3, 15.0))
  }

  it should "work with custom predicates" in {

    // cluster odd/even numbers

    def getNeighbours(point: Point[Double], points: Seq[Point[Double]]): Seq[Point[Double]] = {
      val isEven = point.value(0) % 2 == 0
      points.filterNot(p => p.value(0) % 2 == 0 ^ isEven)
    }

    def isCorePoint(point: Point[Double], neighbours: Seq[Point[Double]]): Boolean = true

    val gdbscan = new GDBSCAN(getNeighbours, isCorePoint)

    val input = DenseMatrix(
      (1.0),
      (2.0),
      (3.0),
      (4.0)
    )
    val cluster = gdbscan cluster input
    val clusterPoints = cluster.map(_.points.map(_.value.toArray))

    cluster.size shouldBe 2
    clusterPoints(0) should contain only (Array(1.0), Array(3.0))
    clusterPoints(1) should contain only (Array(2.0), Array(4.0))
  }
}