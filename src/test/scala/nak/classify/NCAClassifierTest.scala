
package nak.classify

import breeze.linalg.DenseVector
import nak.classify.Classifier.Trainer
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
class NCAClassifierTest
  extends NearestNeighborTestHarness {
  def trainer[L]: Trainer[L, DenseVector[Double]] =
    new NCA.DenseTrainer[L]()
}