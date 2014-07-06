package nak.classify

import breeze.linalg.{minMax, diag, CSCMatrix, SparseVector}
import breeze.storage.Zero
import nak.data.Example

/**
 * nak
 * 6/30/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
object CSCInitializers {
  trait CSCInitializer[L,U] {
    def init(data: Iterable[Example[L, SparseVector[Double]]]): U
  }

  trait ZeroSparseInitializer[L] extends CSCInitializer[L, CSCMatrix[Double]] {
    override def init(data: Iterable[Example[L, SparseVector[Double]]]): CSCMatrix[Double] = {
      val fSize = data.head.features.length
      CSCMatrix.zeros[Double](fSize, fSize)
    }
  }

  trait ScaledDiagSparseInitializer[L] extends CSCInitializer[L, CSCMatrix[Double]] {
    override def init(data: Iterable[Example[L, SparseVector[Double]]]): CSCMatrix[Double] = {
      val fSize = data.head.features.length
      val maxes = new SparseVector[Double](Array.empty,Array.empty[Double],0,fSize)(Zero[Double](Double.NegativeInfinity))
      val mins = new SparseVector[Double](Array.empty,Array.empty[Double],0,fSize)(Zero[Double](Double.PositiveInfinity))

      for (ex <- data; (i,d) <- ex.features.activeIterator) {
        if (maxes(i) < d)
          maxes.update(i,d)
        if (mins(i) > d)
          mins.update(i,d)
      }

      val cscBuilder = new CSCMatrix.Builder[Double](fSize,fSize)
      val maxI = maxes.activeIterator
      val minI = mins.activeIterator
      for (i <- 0 until fSize) {
        cscBuilder.add(i,i,1.0 / (maxes(i) - mins(i)))
      }
      cscBuilder.result(true,true)
    }
  }
}
