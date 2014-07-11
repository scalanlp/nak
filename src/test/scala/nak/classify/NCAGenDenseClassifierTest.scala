package nak.classify

import breeze.linalg.{CSCMatrix, SparseVector, DenseMatrix, DenseVector}
import breeze.math.{MutableInnerProductSpace, TensorSpace}
import nak.classify.Classifier.Trainer
import nak.classify.Initializers.CSCInitializers.ScaledDiagSparseInitializer
import nak.classify.Initializers.DenseInitializers.ScaledDiagDenseInitializer
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * nak
 * 7/8/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
//@RunWith(classOf[JUnitRunner])
//class NCAGenDenseClassifierTest
//  extends DenseNearestNeighborTestHarness {
//  var i = 0
//  def trainer[L]: Trainer[L, DenseVector[Double]] = {
//    new NCA.Trainer[L, DenseVector[Double], DenseMatrix[Double]]() with ScaledDiagDenseInitializer[L]
//  }
//}
