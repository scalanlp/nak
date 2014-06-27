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
import nak.data.Example
import nak.space.dm.DMImplicits.decomposedMahalanobis

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
                 map({case (e, j) => (e.label, decomposedMahalanobis(e.features, ex.features, A))})
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
  trait NCADistanceInitializer[L] {
    def init(data: Iterable[Example[L, DenseVector[Double]]]): DenseMatrix[Double]
  }
  class RandInitializer[L](opt: OptParams = OptParams(), K: Int = 3) extends NCADistanceInitializer[L] {
    override def init(data: Iterable[Example[L, DenseVector[Double]]]): DenseMatrix[Double] = {
      val fSize = data.head.features.length
      DenseMatrix.rand[Double](fSize, fSize) / 50.0
    }
  }

  trait ZeroInitializer[L] extends NCADistanceInitializer[L] {
    override def init(data: Iterable[Example[L, DenseVector[Double]]]): DenseMatrix[Double] = {
      val fSize = data.head.features.length
      DenseMatrix.zeros[Double](fSize, fSize)
    }
  }

  trait ScaledDiagInitializer[L] extends NCADistanceInitializer[L] {
    override def init(data: Iterable[Example[L, DenseVector[Double]]]): DenseMatrix[Double] = {
      val fSize = data.head.features.length
      val scaleDiffs = data.map(_.features.toScalaVector())
                       .foldLeft(Seq.fill(fSize)((Double.PositiveInfinity, Double.NegativeInfinity)))(
          (minmax, fv) =>
            minmax.zip(fv).map(
            {
              case ((min, max), fvVal) => if (fvVal < min) (fvVal, max)
                                          else if (fvVal > max) (min, fvVal)
                                          else (min, max)
            }))
                       .map({case (min, max) => 1.0 / (max - min)})
      diag(DenseVector(scaleDiffs: _*))
    }
  }

  class DenseTrainerOpt[L](opt: OptParams = OptParams(),
                           K: Int = 1)
                          (implicit vspace: MutableInnerProductSpace[DenseVector[Double], Double],
                           canTraverse: CanTraverseValues[DenseVector[Double], Double],
                           man: ClassTag[DenseVector[Double]]) extends Classifier.Trainer[L, DenseVector[Double]] {
    self: NCADistanceInitializer[L] =>

    import vspace._

    override type MyClassifier = NCA[L]

    def eNSqProjNorm(v1: DenseVector[Double], v2: DenseVector[Double], proj: DenseMatrix[Double]) =
      exp(-pow(norm((proj * v1) - (proj * v2)), 2))

    override def train(data: Iterable[Example[L, DenseVector[Double]]]): MyClassifier = {
      val size = data.size
      val featureSize = data.head.features.length
      val initial: DenseMatrix[Double] = init(data)
      val iData = data.map(_.features).toIndexedSeq
      val iLabel = data.map(_.label).toIndexedSeq

      //      def p_ij(i: Int, j: Int, A: DenseMatrix[Double]): Double = {
      //        eNSqProjNorm(iData(i), iData(j), A) /
      //          sum((0 until size).withFilter(_ != i).map(k => exp(-sqProjNorm(iData(i), iData(k), A))))
      //      }
      var iter = 0

      val df = new StochasticDiffFunction[DenseMatrix[Double]] {
        override def calculate(A: DenseMatrix[Double]): (Double, DenseMatrix[Double]) = {

          val i = iter % size
          iter += 1

          val smNorm = (0 until size).withFilter(_ != i).map(k => eNSqProjNorm(iData(i), iData(k), A)).sum

          val smax = DenseVector.tabulate[Double](size)(k => {
            if (k == i) 0.0
            else eNSqProjNorm(iData(i), iData(k), A) / smNorm
          })

          // cache p_i
          val p_i =
            (0 until size).
            withFilter(j => iLabel(j) == iLabel(i)).
            map(j => smax(j)).sum

          //Expected number of points correctly classified, negated for minimization
          val value: Double = -p_i

          // gradient, negated for minimization
          val grad: DenseMatrix[Double] = {

            def term(j: Int) = {
              val diff = iData(i) - iData(j)
              diff * diff.t * smax(j)
            }

            val (first, second) = (0 until size).foldLeft(
              (DenseMatrix.zeros[Double](featureSize, featureSize),
                DenseMatrix.zeros[Double](featureSize, featureSize)))({
              case ((f, s), k) =>
                val kTerm = term(k)
                (f :+ kTerm, if (iLabel(k) == iLabel(i)) s :+ kTerm else s)
            })
//            (A * -2.0) *
            (- A) * ((first :* p_i) - second) //(0 until size).map(i => (first(i) - second(i)) * p_i(i)).reduce(_ + _)
          }

          (value, grad)
        }
      }

      implicit val matvecIso = new Isomorphism[DenseMatrix[Double], DenseVector[Double]] {
        val (r, c) = (initial.rows, initial.cols)

        override def forward(t: DenseMatrix[Double]): DenseVector[Double] = t.toDenseVector

        override def backward(u: DenseVector[Double]): DenseMatrix[Double] = u.asDenseMatrix.reshape(r, c)
      }
      val A =
        matvecIso.backward(new StochasticGradientDescent[DenseVector[Double]](4.0,opt.maxIterations, minIter = 100000) {
          type History = Unit
          def initialHistory(f: StochasticDiffFunction[DenseVector[Double]],init: DenseVector[Double])= ()
          def updateHistory(newX: DenseVector[Double], newGrad: DenseVector[Double], newValue: Double, f: StochasticDiffFunction[DenseVector[Double]], oldState: State) = ()
        }.minimize(df.throughLens[DenseVector[Double]],matvecIso.forward(initial)))
//          StochasticGradientDescent[DenseVector[Double]](initialStepSize = 0.1, maxIter = opt.maxIterations)
//          .minimize(df.throughLens[DenseVector[Double]], matvecIso.forward(initial)))
      println(s"iteR: $iter")
      new NCA[L](data, K, A)
    }
  }


  class DenseTrainer[L](opt: OptParams = OptParams(), K: Int = 3)
                       (implicit vspace: MutableInnerProductSpace[DenseVector[Double], Double],
                        canTraverse: CanTraverseValues[DenseVector[Double], Double],
                        man: ClassTag[DenseVector[Double]]) extends Classifier.Trainer[L, DenseVector[Double]] {

    import vspace._

    override type MyClassifier = NCA[L]

    def sqProjNorm(v1: DenseVector[Double], v2: DenseVector[Double], proj: DenseMatrix[Double]) =
      pow(norm((proj * v1) - (proj * v2)), 2)


    override def train(data: Iterable[Example[L, DenseVector[Double]]]): MyClassifier = {
      val size = data.size
      val featureSize = data.head.features.length
      val indexedData = data.view.map(_.features).toIndexedSeq
      val indexedLabels = data.view.map(_.label).toIndexedSeq

      def p_ij(i: Int, j: Int, A: DenseMatrix[Double]): Double = {
        exp(-sqProjNorm(indexedData(i), indexedData(j), A)) /
          sum((0 until size).withFilter(_ != i).map(k => exp(-sqProjNorm(indexedData(i), indexedData(k), A))))
      }

      var A = DenseMatrix.rand[Double](featureSize, featureSize)
      var lastVal: Double = Double.NegativeInfinity

      println(s"Params:")
      println(s"\tIterations: ${opt.maxIterations}")
      println(s"\tAlpha: ${opt.alpha}")
      println(s"\tTolerance: ${opt.tolerance}")

      for (iter <- 1 to opt.maxIterations) {
        print(s"Iter: $iter")
        val p_i = (0 until size).map(i =>
          sum((0 until size).
              withFilter(j => indexedLabels(j) == indexedLabels(i) && i != j).
              map(j => p_ij(i, j, A))))

        val value: Double = p_i.sum

        println(s"\tVal: $value")

        if (abs(value - lastVal) < 0.001) return new NCA[L](data, K, A)

        lastVal = value

        val grad: DenseMatrix[Double] = {

          def subTDiff(i: Int, j: Int) = {
            val diff = indexedData(i) - indexedData(j)
            diff * diff.t
          }

          def first(i: Int): DenseMatrix[Double] =
            (0 until size).map(k => subTDiff(i, k) * p_ij(i, k, A)).reduce(_ + _)

          def second(i: Int): DenseMatrix[Double] =
            (0 until size).withFilter(j => indexedLabels(j) == indexedLabels(i) && i != j).
            map(j => subTDiff(i, j) * p_ij(i, j, A)).reduce(_ + _)

          (A * 2.0) * (0 until size).map(i => (first(i) - second(i)) * p_i(i)).reduce(_ + _)
        }

        println(s"\tGrad: $grad")

        A :+= A * grad * (1.0 / (iter))
      }

      new NCA[L](data, K, A)
    }
  }


  class CStyleDenseTrainer[L](opt: OptParams = OptParams(), K: Int = 3)
                             (implicit vspace: MutableInnerProductSpace[DenseVector[Double], Double],
                              canTraverse: CanTraverseValues[DenseVector[Double], Double],
                              man: ClassTag[DenseVector[Double]]) extends Classifier.Trainer[L, DenseVector[Double]] {
    self: NCADistanceInitializer[L] =>

    import vspace._

    override type MyClassifier = NCA[L]

    override def train(data: Iterable[Example[L, DenseVector[Double]]]): MyClassifier = {

      val size = data.size
      val initial = init(data)
      val featureSize = data.head.features.length
      val indexedData = data.view.map(_.features).toIndexedSeq
      val indexedLabels = data.view.map(_.label).toIndexedSeq

      var A = initial
      for (iter <- 0 to opt.maxIterations) {
        val i = iter % size

        var softmax_norm = 0.0
        for (k <- 0 until size) {
          softmax_norm += {
            if (k == i) 0.0
            else exp(-pow(norm((A * indexedData(i)) - (A * indexedData(k))), 2))
          }
        }
        val smax = DenseVector.tabulate[Double](size)(k => {
          if (k == i) 0.0
          else exp(-pow(norm((A * indexedData(i)) - (A * indexedData(k))), 2)) / softmax_norm
        })

        val p = (0 until size).withFilter(k => indexedLabels(k) == indexedLabels(i)).map(smax(_)).sum

        val firstTerm = DenseMatrix.zeros[Double](featureSize, featureSize)
        val secondTerm = DenseMatrix.zeros[Double](featureSize, featureSize)
        for (k <- (0 until size).withFilter(k => k != i)) {
          val xik = indexedData(i) - indexedData(k)
          val term = (xik * xik.t) * smax(k)

          firstTerm :+= term
          if (indexedLabels(k) == indexedLabels(i))
            secondTerm :+= term
        }
        firstTerm :*= p

        A :+= A * (firstTerm - secondTerm) * 0.001
      }
      new NCA[L](data, K, A)
    }
  }

}
