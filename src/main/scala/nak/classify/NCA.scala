
package nak.classify

import breeze.collection.mutable.Beam
import breeze.linalg.support.CanTraverseValues
import breeze.linalg._
import breeze.math.MutableInnerProductSpace
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize._
import nak.classify.CSCInitializers.CSCInitializer
import nak.classify.DenseInitializers.DenseInitializer
import nak.data.Example
import nak.space.DMImplicits
import DMImplicits.decomposedMahalanobis
import nak.space.nca.NCAObjectives.DenseObjectives.{NCABatchObjective, NCAStochasticOnlineObjective}
import nak.space.nca.NCAObjectives.{Iso_CSC_SV, Iso_DM_DV}
import nak.space.nca.NCAObjectives.SparseObjectives.NCASparseBatchObjective

import scala.reflect.ClassTag

/**
 * dialogue
 * 6/19/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
class NCA[L,T <: Vector[Double],M <: Matrix[Double]](examples: Iterable[Example[L, T]], k: Int,
             A: M) extends Classifier[L, T] {
  // Iterable of (example, distance) tuples
  type DistanceResult = Iterable[(Example[L, T], Double)]

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
    }).count(identity _).toDouble / examples.size
  }

  /*
   * Additional method to extract distances of k nearest neighbors
   */
  def distances(o: T): DistanceResult = {
    val beam = Beam[(Example[L, T], Double)](k)(Ordering.by(-(_: (_, Double))._2))
    beam ++= examples.map(e => (e, decomposedMahalanobis(e.features, o, A)))
  }

  /** For the observation, return the max voting label with prob = 1.0
    */
  override def scores(o: T): Counter[L, Double] = {
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

//  class DenseTrainerSGD[L](maxIter: Int = 100000, stepSize: Double = 0.001, K: Int = 1)
//                          (implicit vspace: MutableInnerProductSpace[DenseVector[Double], Double],
//                           canTraverse: CanTraverseValues[DenseVector[Double], Double],
//                           man: ClassTag[DenseVector[Double]]) extends Classifier.Trainer[L, DenseVector[Double]] {
//    self: DenseInitializer[L, DenseMatrix[Double]] =>
//
//    import vspace._
//
//    override type MyClassifier = NCA[L]
//
//    override def train(data: Iterable[Example[L, DenseVector[Double]]]): MyClassifier = {
//      val initial: DenseMatrix[Double] = init(data)
//
//      //      def p_ij(i: Int, j: Int, A: DenseMatrix[Double]): Double = {
//      //        eNSqProjNorm(iData(i), iData(j), A) /
//      //          sum((0 until size).withFilter(_ != i).map(k => exp(-sqProjNorm(iData(i), iData(k), A))))
//      //      }
//
//      val df = new NCAStochasticOnlineObjective[L](data)
//
//      implicit val mvIso = new Iso_DM_DV(initial.rows, initial.cols)
//
//      val A: DenseMatrix[Double] = mvIso.backward(new StochasticGradientDescent[DenseVector[Double]](stepSize, maxIter) {
//        type History = Unit
//
//        def initialHistory(f: StochasticDiffFunction[DenseVector[Double]], init: DenseVector[Double]) = ()
//
//        def updateHistory(newX: DenseVector[Double], newGrad: DenseVector[Double], newValue: Double, f: StochasticDiffFunction[DenseVector[Double]], oldState: State) = ()
//
//        override def determineStepSize(state: State, f: StochasticDiffFunction[DenseVector[Double]], dir: DenseVector[Double]): Double = defaultStepSize
//      }.minimize(df.throughLens[DenseVector[Double]], mvIso.forward(initial)))
//
//      new NCA[L](data, K, A)
//    }
//  }

  class DenseTrainer[L](opt: OptParams = OptParams(), K: Int = 1)
                          (implicit vspace: MutableInnerProductSpace[DenseVector[Double], Double],
                           canTraverse: CanTraverseValues[DenseVector[Double], Double],
                           man: ClassTag[DenseVector[Double]]) extends Classifier.Trainer[L, DenseVector[Double]] {
    self: DenseInitializer[L, DenseMatrix[Double]] =>

    import vspace._

    override type MyClassifier = NCA[L,DenseVector[Double], DenseMatrix[Double]]

    override def train(data: Iterable[Example[L, DenseVector[Double]]]): MyClassifier = {

      val initial: DenseMatrix[Double] = init(data)

      val df = new NCABatchObjective[L](data)

      implicit val mvIso = new Iso_DM_DV(initial.rows, initial.cols)

      val A: DenseMatrix[Double] = mvIso.backward(opt.minimize(df.throughLens[DenseVector[Double]], mvIso.forward(initial)))

      new NCA[L, DenseVector[Double], DenseMatrix[Double]](data, K, A)
    }
  }

  class SparseTrainer[L](opt: OptParams = OptParams(), K: Int = 1)
                          (implicit vspace: MutableInnerProductSpace[SparseVector[Double], Double],
                           canTraverse: CanTraverseValues[SparseVector[Double], Double],
                           man: ClassTag[SparseVector[Double]]) extends Classifier.Trainer[L, SparseVector[Double]] {
    self: CSCInitializer[L, CSCMatrix[Double]] =>

    import vspace._

    override type MyClassifier = NCA[L, SparseVector[Double], CSCMatrix[Double]]

    override def train(data: Iterable[Example[L, SparseVector[Double]]]): MyClassifier = {

      val initial: CSCMatrix[Double] = init(data)

      val df = new NCASparseBatchObjective[L](data)

      implicit val mvIso = new Iso_CSC_SV(initial.rows, initial.cols)

      val A: CSCMatrix[Double] = mvIso.backward(opt.minimize(df.throughLens[SparseVector[Double]], mvIso.forward(initial)))

      new NCA[L,SparseVector[Double], CSCMatrix[Double]](data, K, A)
    }
  }
}
