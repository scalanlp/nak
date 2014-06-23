/*
 * dialogue -- The University of Washington Dialogue Framework
 *
 * Copyright 2013 - Gabriel Schubiner
 *
 * NCA.scala is part of dialogue.
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

import breeze.linalg.support.CanTraverseValues
import breeze.linalg._
import breeze.math.MutableInnerProductSpace
import breeze.numerics.exp
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize.{StochasticGradientDescent, StochasticDiffFunction}
import breeze.util.Isomorphism
import nak.data.Example
import nak.space.dm.DMImplicits.decomposedMahalanobis

import scala.reflect.ClassTag

/**
 * dialogue
 * 6/19/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
class NCA[L,T](examples: Iterable[Example[L,T]], A: DenseMatrix[Double]) extends Classifier[L,T] {
  /** For the observation, return the score for each label that has a nonzero
    * score.
    */
  override def scores(o: T): Counter[L, Double] = ???
}

object NCA {

  class DenseTrainer[L](opt: OptParams = OptParams())
                    (implicit vspace: MutableInnerProductSpace[DenseVector[Double], Double],
                     canTraverse: CanTraverseValues[DenseVector[Double],Double],
                     man: ClassTag[DenseVector[Double]]) extends Classifier.Trainer[L,DenseVector[Double]] {

    import vspace._

    override type MyClassifier = NCA[L,DenseVector[Double]]

    def whitening_transform(data: Iterable[Example[L,DenseVector[Double]]]): DenseMatrix[Double] = {
      minMax(data.flatMap(_.features))
    }

    override def train(data: Iterable[Example[L, DenseVector[Double]]]): MyClassifier = {
      val indexedData = data.zipWithIndex.toArray
      val df = new StochasticDiffFunction[DenseMatrix[Double]] {

        def p_ij(i: Int,j: Int, A: DenseMatrix[Double]): Double = {
          exp(- decomposedMahalanobis(indexedData(i)._1.features,indexedData(j)._1.features,A)) /
            sum(indexedData.withFilter(_._2 != i).map(xk => exp(- decomposedMahalanobis(indexedData(i)._1.features,xk._1.features,A))))
        }

        override def calculate(A: DenseMatrix[Double]): (Double, DenseMatrix[Double]) = {

          // cache p_i
          val p_i = indexedData.map(xi => sum(indexedData.withFilter(_._1.label == xi._1.label).map(xj => decomposedMahalanobis(xi._1.features,xj._1.features,A))))

          //Expected number of points correctly classified
          val value: Double = sum(p_i)

          val grad: DenseMatrix[Double] = {
            A * (2 * sum(indexedData.map(xii =>
              (p_i(xii._2) * sum(indexedData.map(xk =>
                ((xii._1.features - xk._1.features) * (xii._1.features - xk._1.features).t) * p_ij(xii._2,xk._2,A)))) -
              sum(indexedData.filter(_._1.label == xii._1.label).map(xj =>
                ((xii._1.features - xj._1.features) * (xii._1.features - xj._1.features).t) * p_ij(xii._2,xj._2,A))))))
          }

          (value, grad)
        }
      }

      val sgd = StochasticGradientDescent().minimize(df,DenseMatrix.zeros())

      val A: DenseMatrix[Double] = ???
      new NCA[L,DenseVector[Double]](data, A)
    }
  }
}
