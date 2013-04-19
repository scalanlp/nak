package nak.core

import org.scalatest.FunSpec

import nak.data.TwoPassDataIndexer
import nak.util.PrepAttachDataUtil.createTrainingStream
import nak.util.PrepAttachDataUtil.testModel


/**
 * Test liblinear training on the prepositional phrase attachment code.
 */ 
class ClassifierSpec extends FunSpec {

  describe("Liblinear Training") {

    it ("should maintain accuracy for PPA data") {
      val dataIndexer = new TwoPassDataIndexer(createTrainingStream(), 1)
      val config = new nak.liblinear.LiblinearConfig()
      val model = nak.NakContext.trainClassifier(config,dataIndexer)
      testModel(model, 0.8202525377568705)
    }

  }

}
