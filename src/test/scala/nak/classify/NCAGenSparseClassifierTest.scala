package nak.classify

import breeze.linalg.{CSCMatrix, SparseVector}
import breeze.math.{MutableCoordinateSpace, TensorSpace}
import nak.classify.Classifier.Trainer
import nak.classify.Initializers.CSCInitializers.ScaledDiagSparseInitializer
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
    implicit val mspace = TensorSpace.make[CSCMatrix[Double],(Int,Int),Double]
    new NCA.Trainer[L, SparseVector[Double], CSCMatrix[Double]]() with ScaledDiagSparseInitializer[L]
  }
}

