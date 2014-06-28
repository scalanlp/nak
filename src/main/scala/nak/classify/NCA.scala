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

import breeze.collection.mutable.Beam
import breeze.linalg.support.CanTraverseValues
import breeze.linalg._
import breeze.math.MutableInnerProductSpace
import breeze.numerics.{abs, pow, exp}
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize._
import breeze.util.Isomorphism
import nak.classify.DenseInitializers.Initializer
import nak.data.Example
import nak.space.dm.DMImplicits.decomposedMahalanobis
import nak.space.nca.NCAObjectives.DenseObjectives.{NCABatchObjective, NCAStochasticOnlineObjective}
import nak.space.nca.NCAObjectives.Iso.Iso_DM_DV

import scala.reflect.ClassTag
import scala.util.Random

/**
 * dialogue
 * 6/19/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
class NCA[L](examples: Iterable[Example[L, DenseVector[Double]]], k: Int,
             A: DenseMatrix[Double]) extends Classifier[L, DenseVector[Double]] {
  // Iterable of (example, distance) tuples
  type DistanceResult = Iterable[(Example[L, DenseVector[Double]], Double)]

  val projection = A

  def testLOO(): Double = {
    val indexedExamples = examples.zipWithIndex
    indexedExamples.map({
      case (ex, i) =>
        val beam = Beam[(L, Double)](k)(Ordering.by(-(_: (_, Double))._2))
        beam ++= indexedExamples.
          withFilter(_._2 != i).
          map({ case (e, j) => (e.label, decomposedMahalanobis(e.features, ex.features, A))})
        beam.groupBy(_._1).maxBy(_._2.size)._1 == ex.label
    }).count(identity).toDouble / examples.size
  }

  /*
   * Additional method to extract distances of k nearest neighbors
   */
  def distances(o: DenseVector[Double]): DistanceResult = {
    val beam = Beam[(Example[L, DenseVector[Double]], Double)](k)(Ordering.by(-(_: (_, Double))._2))
    beam ++= examples.map(e => (e, decomposedMahalanobis(e.features, o, A)))
  }

  /** For the observation, return the max voting label with prob = 1.0
    */
  override def scores(o: DenseVector[Double]): Counter[L, Double] = {
    // Beam reverses ordering from min heap to max heap, but we want min heap
    // since we are tracking distances, not scores.
    val beam = Beam[(L, Double)](k)(Ordering.by(-(_: (_, Double))._2))

    // Add all examples to beam, tracking label and distance from testing point
    beam ++= examples.map(e => (e.label, decomposedMahalanobis(e.features, o, A)))

    // Max voting classification rule
    val predicted = beam.groupBy(_._1).maxBy(_._2.size)._1

    // Degenerate discrete distribution with prob = 1.0 at predicted label
    Counter((predicted, 1.0))
  }

}

object NCA {

  class DenseTrainerSGD[L](maxIter: Int = 100000, stepSize: Double = 0.001, K: Int = 1)
                          (implicit vspace: MutableInnerProductSpace[DenseVector[Double], Double],
                           canTraverse: CanTraverseValues[DenseVector[Double], Double],
                           man: ClassTag[DenseVector[Double]]) extends Classifier.Trainer[L, DenseVector[Double]] {
    self: Initializer[L, DenseMatrix[Double]] =>

    import vspace._

    override type MyClassifier = NCA[L]

    override def train(data: Iterable[Example[L, DenseVector[Double]]]): MyClassifier = {
      val initial: DenseMatrix[Double] = init(data)

      //      def p_ij(i: Int, j: Int, A: DenseMatrix[Double]): Double = {
      //        eNSqProjNorm(iData(i), iData(j), A) /
      //          sum((0 until size).withFilter(_ != i).map(k => exp(-sqProjNorm(iData(i), iData(k), A))))
      //      }

      val df = new NCAStochasticOnlineObjective[L](data)

      implicit val mvIso = new Iso_DM_DV(initial.rows, initial.cols)

      val A: DenseMatrix[Double] = mvIso.backward(new StochasticGradientDescent[DenseVector[Double]](stepSize, maxIter) {
        type History = Unit

        def initialHistory(f: StochasticDiffFunction[DenseVector[Double]], init: DenseVector[Double]) = ()

        def updateHistory(newX: DenseVector[Double], newGrad: DenseVector[Double], newValue: Double, f: StochasticDiffFunction[DenseVector[Double]], oldState: State) = ()

        override def determineStepSize(state: State, f: StochasticDiffFunction[DenseVector[Double]], dir: DenseVector[Double]): Double = defaultStepSize
      }.minimize(df.throughLens[DenseVector[Double]], mvIso.forward(initial)))

      new NCA[L](data, K, A)
    }
  }

  class DenseTrainerOpt[L](opt: OptParams = OptParams(), K: Int = 1)
                          (implicit vspace: MutableInnerProductSpace[DenseVector[Double], Double],
                           canTraverse: CanTraverseValues[DenseVector[Double], Double],
                           man: ClassTag[DenseVector[Double]]) extends Classifier.Trainer[L, DenseVector[Double]] {
    self: Initializer[L, DenseMatrix[Double]] =>

    import vspace._

    override type MyClassifier = NCA[L]

    override def train(data: Iterable[Example[L, DenseVector[Double]]]): MyClassifier = {

      val initial: DenseMatrix[Double] = init(data)

      val df = new NCABatchObjective[L](data)

      implicit val mvIso = new Iso_DM_DV(initial.rows, initial.cols)

      val A: DenseMatrix[Double] = mvIso.backward(opt.minimize(df.throughLens[DenseVector[Double]], mvIso.forward(initial)))

      new NCA[L](data, K, A)
    }

  }
}
