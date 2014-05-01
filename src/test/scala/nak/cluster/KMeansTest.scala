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

import org.scalatest.FunSuite
import breeze.linalg.{DenseVector,SparseVector}
import breeze.numerics._

/**
  * Test both DenseVector and SparseVector use of K-means.
  */
class KMeansTest extends FunSuite {

  test("K-means DenseVector test") {
    val clusters = IndexedSeq(
      IndexedSeq((0.0, 0.0), (0.0, 1.0), (1.0, 0.0), (1.0, 1.0)),
      IndexedSeq((8.0, 8.0), (8.0, 9.0), (9.0, 8.0), (9.0, 9.0)),
      IndexedSeq((0.0, 8.0), (0.0, 9.0), (1.0, 8.0), (1.0, 9.0))
    ).map(cluster => cluster.map(point => DenseVector(point._1,point._2)))

    val points = clusters.flatten

    val kmeans = new Kmeans[DenseVector[Double]](points, fixedSeedForRandom=true)
    val (dispersion, centroids) = kmeans.run(3,25)
    assert(closeTo(dispersion,6.0))
    val IndexedSeq(a, b, c) = centroids.sortWith{ (a,b) =>
      if(a(0) < b(0)) true else if(a(0) > b(0)) false else a(1) < b(1)
    }

    assert((DenseVector(0.5, 0.5)) === a)
    assert((DenseVector(0.5, 8.5)) === b)
    assert((DenseVector(8.5, 8.5)) === c)
  }


  test("K-means SparseVector test") {
    val clusters = IndexedSeq(
      IndexedSeq((0, 0.0, 1, 0.0), (0, 0.0, 2, 1.0), (1, 1.0, 2, 3.0), (0, 1.0, 2, 1.0)),
      IndexedSeq((0, 8.0, 1, 8.0), (0, 8.0, 2, 9.0), (1, 9.0, 2, 8.0), (0, 9.0, 2, 9.0)),
      IndexedSeq((0, 0.0, 1, 8.0), (0, 0.0, 2, 9.0), (1, 1.0, 2, 8.0), (0, 1.0, 2, 9.0))
    ).map { cluster =>
      cluster.map { point: (Int, Double, Int, Double) =>
        new SparseVector(Array(point._1,point._3), Array(point._2, point._4), 3)(breeze.storage.DefaultArrayValue(0.0))
      }
    }

    val points = clusters.flatten

    val kmeans = new Kmeans[SparseVector[Double]](points, fixedSeedForRandom=true)
    val (dispersion, centroids) = kmeans.run(3,25)

    assert(closeTo(dispersion,175.05))
    val IndexedSeq(a, b, c) = centroids.sortWith{ (a,b) =>
      if (a(0) < b(0)) true
      else if (a(0) > b(0)) false
      else if (a(1) < b(1)) true
      else a(2) > b(2)
    }

    assert((SparseVector(Array(0.25,0.25,1.25))) === a)
    assert((SparseVector(Array(2.6666666666666665,8.333333333333334,2.6666666666666665))) === b)
    assert((SparseVector(Array(3.6,0.2,8.8))) === c)
  }


}
