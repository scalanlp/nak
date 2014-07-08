
package nak.classify

import breeze.linalg.DenseVector
import nak.classify.Classifier.Trainer
import nak.classify.DenseInitializers.ScaledDiagDenseInitializer
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * dialogue
 * 6/23/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
@RunWith(classOf[JUnitRunner])
class NCADenseClassifierTest
  extends DenseNearestNeighborTestHarness {
  def trainer[L]: Trainer[L, DenseVector[Double]] =
    new NCA.DenseTrainer[L]() with ScaledDiagDenseInitializer[L]
}

