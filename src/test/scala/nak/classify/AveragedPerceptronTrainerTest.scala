package nak.classify

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import breeze.linalg._

/**
 *
 * @author gabeos
 */
@RunWith(classOf[JUnitRunner])
class AveragedPerceptronTrainerTest
  extends ClassifierTrainerTestHarness
  with ContinuousTestHarness {
  def trainer[L,T]:Classifier.Trainer[L,Counter[T,Double]] =
    new Perceptron.AveragedTrainer[L,Counter[T,Double]]
}
