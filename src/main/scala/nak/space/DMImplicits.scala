/*
 * dialogue -- The University of Washington Dialogue Framework
 *
 * Copyright 2013 - Gabriel Schubiner
 *
 * DMImplicits.scala is part of dialogue.
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

package nak.space

import breeze.generic.UFunc
import breeze.linalg._
import breeze.linalg.operators.{OpMulMatrix, OpSub}
import breeze.math.MutableInnerProductSpace
import breeze.numerics._

/**
 * dialogue
 * 6/19/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
object DMImplicits {

  type chebyshev = chebyshev.type
  type cosine = cosine.type
  type euclidean = euclidean.type
  type mahalanobis = mahalanobis.type
  type manhattan = manhattan.type
  type minkowski = minkowski.type
  type projectedSquaredNorm = projectedSquaredNorm.type

  object projectedSquaredNorm extends UFunc {
    implicit def pSqNorm[T, U](implicit mulImpl: OpMulMatrix.Impl2[U, T, T],
                               subImpl: OpSub.Impl2[T, T, T],
                               normImpl: norm.Impl[T, Double]): Impl3[T, T, U, Double] =
      new Impl3[T, T, U, Double] {
        def apply(v: T, v2: T, proj: U): Double = {
          pow(norm(subImpl(mulImpl(proj, v), mulImpl(proj, v2))), 2)
        }
      }
  }

  object chebyshev extends UFunc {
    implicit def chebyshevDistanceFromZippedValues[T, U]
    (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl2[T, U, Double] =
      new Impl2[T, U, Double] {
        def apply(v: T, v2: U): Double = {
          var minDist = Double.NegativeInfinity
          zipValues(v, v2).foreach {
            (a, b) =>
              val absdiff = abs(a - b)
              if (absdiff > minDist)
                minDist = absdiff
          }
          minDist
        }
      }
  }

  object cosine extends UFunc {
    implicit def cosineDistanceFromZippedValues[T, U]
    (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl2[T, U, Double] =
      new Impl2[T, U, Double] {
        def apply(v: T, v2: U): Double = {
          var numer = 0.0
          var denom = 0.0
          var denom2 = 0.0
          zipValues(v, v2).foreach {
            (a, b) =>
              numer += a * b
              denom += a * a
              denom2 += b * b
          }
          numer / (sqrt(denom) * sqrt(denom2))
        }
      }
  }

  object euclidean extends UFunc {

    implicit def euclideanDistanceFromZippedValues[T, U]
    (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl2[T, U, Double] =
      new Impl2[T, U, Double] {
        def apply(v: T, v2: U): Double = {
          var dist = 0.0
          zipValues(v, v2).foreach {
            (a, b) =>
              val diff = a - b
              dist += (diff * diff)
          }
          sqrt(dist)
        }
      }
  }


  object decomposedMahalanobis extends UFunc {
    //    implicit def decomposedMahalanobisFromLinearTransformationMatrix[T]
    //    (implicit vspace: MutableInnerProductSpace[T,Double],
    //      ev: T <:< Vector[Double]): Impl3[T,T,DenseMatrix[Double],Double] = {
    //      import vspace._
    //      new Impl3[T, T, DenseMatrix[Double], Double] {
    //        def apply(v: T, v2: T, A: DenseMatrix[Double]): Double = {
    //          ((A * v) - (A * v2)).t * ((A * v) - (A * v2))
    //        }
    //      }
    //    }
    implicit def decomposedMahalanobisFromLinearTransformationDense(implicit vspace: MutableInnerProductSpace[DenseVector[Double], Double]):
    Impl3[DenseVector[Double], DenseVector[Double], DenseMatrix[Double], Double] = {
      import vspace._
      new Impl3[DenseVector[Double], DenseVector[Double], DenseMatrix[Double], Double] {
        def apply(v: DenseVector[Double], v2: DenseVector[Double], A: DenseMatrix[Double]): Double = {
          ((A * v) - (A * v2)) dot ((A * v) - (A * v2))
        }
      }
    }

    implicit def decomposedMahalanobisFromLinearTransformationSparse(implicit vspace: MutableInnerProductSpace[SparseVector[Double], Double]):
    Impl3[SparseVector[Double], SparseVector[Double], CSCMatrix[Double], Double] = {
      import vspace._
      new Impl3[SparseVector[Double], SparseVector[Double], CSCMatrix[Double], Double] {
        def apply(v: SparseVector[Double], v2: SparseVector[Double], A: CSCMatrix[Double]): Double = {
          ((A * v) - (A * v2)) dot ((A * v) - (A * v2))
        }
      }
    }
  }

  object mahalanobis extends UFunc {
    def requireSymmetricMatrix[V](mat: Matrix[V]): Unit = {
      if (mat.rows != mat.cols)
        throw new MatrixNotSquareException

      for (i <- 0 until mat.rows; j <- 0 until i)
        if (mat(i, j) != mat(j, i))
          throw new MatrixNotSymmetricException
    }

    implicit object mahalanobisDistanceFromRawMeanCovValues_DV_DV_DM
      extends Impl3[DenseVector[Double], DenseVector[Double], DenseMatrix[Double], Double] {
      override def apply(v: DenseVector[Double], v2: DenseVector[Double], v3: DenseMatrix[Double]): Double = {
        requireSymmetricMatrix(v3)
        sqrt((v - v2).t * (v3 \ (v - v2)))
      }
    }

    // Assumes rows represent individual data points (i.e. v2 is MxN array comprising M data points in R^N
    //  implicit object mahalanobisDistanceFromPointDistribution_DV_DM extends Impl2[DenseVector[Double],DenseMatrix[Double], Double] {
    //    override def apply(v: DenseVector[Double], v2: DenseMatrix[Double]): Double = {
    //      val means: DenseMatrix[Double] = mean(v2(::, *)) // mean of columns into 1xN Matrix
    //      val centered: DenseMatrix[Double] = v2(*,::) - means
    //      val sampleCov = (1 / v2.rows) :* (centered * centered.t)
    //
    //      mahalanobisDistanceFromRawMeanCovValues_DV_DV_DM(v, means.toDenseVector, sampleCov)
    //    }
    //  }
  }

  object manhattan extends UFunc {
    implicit def manhattanDistanceFromZippedValues[T, U]
    (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl2[T, U, Double] =
      new Impl2[T, U, Double] {
        def apply(v: T, v2: U): Double = {
          var dist = 0.0
          zipValues(v, v2).foreach {
            (a, b) =>
              dist += abs(a - b)
          }
          dist
        }
      }
  }

  object minkowski extends UFunc {
    implicit def minkowskiDistanceFromZippedValues[T, U]
    (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl3[T, U, Double, Double] =
      new Impl3[T, U, Double, Double] {
        def apply(v: T, v2: U, v3: Double): Double = {
          var cumul = 0.0
          zipValues(v, v2).foreach {
            (a, b) =>
              cumul += pow(abs(a - b), v3)
          }
          pow(cumul, 1 / v3)
        }
      }
  }

}
