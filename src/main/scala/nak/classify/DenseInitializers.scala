package nak.classify

import breeze.linalg._
import breeze.optimize.FirstOrderMinimizer.OptParams
import nak.data.Example

/**
 * nak
 * 6/27/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
object DenseInitializers {
  trait Initializer[L,U] {
    def init(data: Iterable[Example[L, DenseVector[Double]]]): U
  }

  trait RandInitializer[L] extends Initializer[L, DenseMatrix[Double]] {
    override def init(data: Iterable[Example[L, DenseVector[Double]]]): DenseMatrix[Double] = {
      val fSize = data.head.features.length
      DenseMatrix.rand[Double](fSize, fSize) / 50.0
    }
  }

  trait ZeroInitializer[L] extends Initializer[L, DenseMatrix[Double]] {
    override def init(data: Iterable[Example[L, DenseVector[Double]]]): DenseMatrix[Double] = {
      val fSize = data.head.features.length
      DenseMatrix.zeros[Double](fSize, fSize)
    }
  }

  trait ScaledDiagInitializer[L] extends Initializer[L, DenseMatrix[Double]] {
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
        .map({ case (min, max) => 1.0 / (max - min)})
      diag(DenseVector(scaleDiffs: _*))
    }
  }
}
