/*
 * dialogue -- The University of Washington Dialogue Framework
 *
 * Copyright 2013 - Gabriel Schubiner
 *
 * mahalanobis.scala is part of dialogue.
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

package nak.space.dm

import breeze.linalg._
import breeze.numerics._
import breeze.stats._
import breeze.generic.UFunc
import breeze.linalg.operators.{OpMulMatrix, OpSolveMatrixBy, OpSub}
import breeze.linalg.support.CanTranspose

/**
 * dialogue
 * 6/8/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *         Multiple implicits for different formulations of Mahalanobis distance
 *         * Vector-Vector distance with specified covariance matrix
 *         * Vector-Matrix distance with Matrix assumed to represent distribution
 *         *
 *
 */
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
