package nak.classify

import breeze.linalg.SparseVector
import nak.classify.Classifier.Trainer
import nak.classify.Initializers.CSCInitializers.ScaledDiagSparseInitializer
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * 7/7/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
@RunWith(classOf[JUnitRunner])
class NCASparseClassifierTest
  extends SparseNearestNeighborTestHarness {
  def trainer[L]: Trainer[L, SparseVector[Double]] =
    new NCA.SparseTrainer[L]() with ScaledDiagSparseInitializer[L]
}
