package nak.space.nca

import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import breeze.math.{TensorSpace, Semiring, MutableInnerProductSpace}
import breeze.numerics._
import breeze.optimize.{BatchDiffFunction, StochasticDiffFunction}
import breeze.util.Isomorphism
import nak.data.Example

import scala.reflect.ClassTag

/**
 * nak
 * 6/27/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *         Different styles of objective functions for NCA
 */
object NCAObjectives {

  class Iso_DM_DV(r: Int, c: Int) extends Isomorphism[DenseMatrix[Double], DenseVector[Double]] {
    override def forward(t: DenseMatrix[Double]): DenseVector[Double] = t.flatten()

    override def backward(u: DenseVector[Double]): DenseMatrix[Double] = u.asDenseMatrix.reshape(r, c)
  }

  class Iso_CSC_SV(r: Int, c: Int) extends Isomorphism[CSCMatrix[Double], SparseVector[Double]] {

    override def forward(t: CSCMatrix[Double]): SparseVector[Double] = t.flatten()

    override def backward(u: SparseVector[Double]): CSCMatrix[Double] = reshape(u, r, c)
  }

//  class Iso_M_V[M,V](r: Int, c: Int)(implicit vView: V <:< Vector[Double], mView: M <:< Matrix[Double]) extends Isomorphism[M,V] {
//    override def forward(t: M): Vector[Double] = t.flatten()
//
//    override def backward(u: V): M = reshape(u,r,c)
//  }

  object Objectives {


    class NCABatchObjective[L, T, M](data: Iterable[Example[L, T]])(implicit vspace: MutableInnerProductSpace[T, Double],
                                                                    mspace: TensorSpace[M, (Int, Int), Double],
                                                                    opMulMV: OpMulMatrix.Impl2[M, T, T],
                                                                    viewM: M <:< Matrix[Double],
                                                                    viewT: T <:< Vector[Double]) extends BatchDiffFunction[M] {

      import mspace._

      val size = data.size
      val featureSize = data.head.features.length
      val iData = data.map(_.features).toIndexedSeq
      val iLabel = data.map(_.label).toIndexedSeq

      override def calculate(A: M, batch: IndexedSeq[Int]): (Double, M) = {

        // shortcut to access indexed data through batch indices
        val batchData = iData.compose(batch.apply)

        val smNorms =
          batch.map(i =>
            (0 until size).withFilter(_ != i).map(k => eNSqProjNorm(iData(i), iData(k), A)).sum)

        val smaxes = Array.tabulate[Double](batch.size, batch.size)((i, k) => {
          if (i == k) 0.0
          else eNSqProjNorm(batchData(i), batchData(k), A) / smNorms(i)
        })

        def term(i: Int, j: Int): M = {
          val diff = iData(i) - iData(j)
          val ddt = diff * diff.t
          (diff * diff.t) * smaxes(i)(j)
        }

        var value = 0.0
        val grad = zeros(A)
        var i = 0
        while (i < batch.size) {
          val ind = batch(i)

          var jt = 0
          while (iLabel(ind) != iLabel(jt)) jt += 1
          val tt = term(ind, jt)
          var f = tt
          var s = tt
          var p_ind = smaxes(ind)(jt)

          var j = 1
          while (j < size) {
            if (j != jt) {
              val kTerm = term(ind, j)
              f = f :+ kTerm
              if (iLabel(ind) == iLabel(j)) {
                s = s :+ kTerm
                p_ind += smaxes(ind)(j)
              }
            }
            j += 1
          }
          value += p_ind
          grad :+= ((f :* p_ind) - s)

          i += 1
        }

        (value,(A :* grad) * -2.0)
      }

      override def fullRange: IndexedSeq[Int] = 0 until size

      private def eNSqProjNorm(v1: T, v2: T, proj: M): Double = {
        exp(-pow(norm((proj * v1) - (proj * v2)), 2))
      }
    }

  }

  object DenseObjectives {

    private def eNSqProjNorm(v1: DenseVector[Double], v2: DenseVector[Double], proj: DenseMatrix[Double]) =
      exp(-pow(norm((proj * v1) - (proj * v2)), 2))

    class NCAStochasticOnlineObjective[L](data: Iterable[Example[L, DenseVector[Double]]]) extends StochasticDiffFunction[DenseMatrix[Double]] {
      val size = data.size
      val featureSize = data.head.features.length
      val iData = data.map(_.features).toIndexedSeq
      val iLabel = data.map(_.label).toIndexedSeq
      var iter = 0

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
          (A * -2.0) * ((first :* p_i) - second) //(0 until size).map(i => (first(i) - second(i)) * p_i(i)).reduce(_ + _)
        }

        (value, grad)
      }
    }

    class NCABatchObjective[L](data: Iterable[Example[L, DenseVector[Double]]]) extends BatchDiffFunction[DenseMatrix[Double]] {
      val size = data.size
      val featureSize = data.head.features.length
      val iData = data.map(_.features).toIndexedSeq
      val iLabel = data.map(_.label).toIndexedSeq

      override def calculate(A: DenseMatrix[Double], batch: IndexedSeq[Int]): (Double, DenseMatrix[Double]) = {

        // shortcut to access indexed data through batch indices
        val batchData = iData.compose(batch.apply)

        val smNorms =
          batch.map(i =>
            (0 until size).withFilter(_ != i).map(k => eNSqProjNorm(iData(i), iData(k), A)).sum)

        val smaxes = Array.tabulate[Double](batch.size, batch.size)((i, k) => {
          if (i == k) 0.0
          else eNSqProjNorm(batchData(i), batchData(k), A) / smNorms(i)
        })

        def term(i: Int, j: Int) = {
          val diff = iData(i) - iData(j)
          val ddt = diff * diff.t
          diff * diff.t * smaxes(i)(j)
        }

        def scaleNegate(gv: (Double, DenseMatrix[Double])) = (-gv._1, (A * gv._2) * -2.0)

        scaleNegate(batch.foldLeft((0.0, DenseMatrix.zeros[Double](featureSize, featureSize)))({
          case ((vAgg, gAgg), i) => {
            val (first, second, p_i) = (0 until size).foldLeft(
              (DenseMatrix.zeros[Double](featureSize, featureSize),
                DenseMatrix.zeros[Double](featureSize, featureSize), 0.0))({
              case ((f, s, p), j) =>
                val kTerm = term(i, j)
                if (iLabel(j) == iLabel(i))
                  (f :+ kTerm, s :+ kTerm, p + smaxes(i)(j))
                else (f :+ kTerm, s, p)
            })
            (vAgg + p_i, gAgg + ((first :* p_i) - second))
          }
        }))
      }

      override def fullRange: IndexedSeq[Int] = 0 until size
    }

  }

  object SparseObjectives {
    private def eNSqProjNorm(v1: SparseVector[Double], v2: SparseVector[Double], proj: CSCMatrix[Double]) =
      exp(-pow(norm((proj * v1) - (proj * v2)), 2))

    class NCASparseBatchObjective[L](data: Iterable[Example[L, SparseVector[Double]]]) extends BatchDiffFunction[CSCMatrix[Double]] {
      val size = data.size
      val featureSize = data.head.features.length
      val iData = data.map(_.features).toIndexedSeq
      val iLabel = data.map(_.label).toIndexedSeq

      override def calculate(A: CSCMatrix[Double], batch: IndexedSeq[Int]): (Double, CSCMatrix[Double]) = {

        // shortcut to access indexed data through batch indices
        val batchData = iData.compose(batch.apply)

        val smNorms =
          batch.map(i =>
            (0 until size).withFilter(_ != i).map(k => eNSqProjNorm(iData(i), iData(k), A)).sum)

        val smaxes = Array.tabulate[Double](batch.size, batch.size)((i, k) => {
          if (i == k) 0.0
          else eNSqProjNorm(batchData(i), batchData(k), A) / smNorms(i)
        })

        def term(i: Int, j: Int) = {
          val diff = iData(i) - iData(j)
          (diff.asCSCMatrix().t * diff.t) * smaxes(i)(j)
        }

        def scaleNegate(gv: (Double, CSCMatrix[Double])) = (-gv._1, (A * gv._2) * -2.0)

        scaleNegate(batch.foldLeft((0.0, CSCMatrix.zeros[Double](featureSize, featureSize)))({
          case ((vAgg, gAgg), i) => {
            val (first, second, p_i) = (0 until size).foldLeft(
              (CSCMatrix.zeros[Double](featureSize, featureSize),
                CSCMatrix.zeros[Double](featureSize, featureSize), 0.0))({
              case ((f, s, p), j) =>
                val kTerm = term(i, j)
                if (iLabel(j) == iLabel(i))
                  (f :+ kTerm, s :+ kTerm, p + smaxes(i)(j))
                else (f :+ kTerm, s, p)
            })
            (vAgg + p_i, gAgg + ((first :* p_i) - second))
          }
        }))
      }

      override def fullRange: IndexedSeq[Int] = 0 until size
    }

  }

}
