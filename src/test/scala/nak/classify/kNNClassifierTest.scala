
package nak.classify

import breeze.linalg.DenseVector
import nak.classify.Classifier.Trainer
import nak.space.DMImplicits
import DMImplicits.euclidean
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * dialogue
 * 6/19/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
@RunWith(classOf[JUnitRunner])
class kNNClassifierTest
  extends DenseNearestNeighborTestHarness {
  def trainer[L]: Trainer[L, DenseVector[Double]] =
    new kNearestNeighbor.Trainer[L, DenseVector[Double], euclidean](3)
}
