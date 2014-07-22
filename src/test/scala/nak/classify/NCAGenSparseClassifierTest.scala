package nak.classify

import breeze.linalg.{CSCMatrix, SparseVector}
import nak.classify.Classifier.Trainer
import nak.classify.Initializers.GenericScaledDiagInitializer
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * nak
 * 7/8/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
@RunWith(classOf[JUnitRunner])
class NCAGenSparseClassifierTest
  extends DenseNearestNeighborTestHarness {
  var i = 0
  def trainer[L]: Trainer[L, SparseVector[Double]] = {
    import CSCMatrix.FrobeniusInnerProductCSCMatrixSpace._
    new NCA.Trainer[L, SparseVector[Double], CSCMatrix[Double]]() with GenericScaledDiagInitializer[L,SparseVector[Double],CSCMatrix[Double]]
  }
}

