/*
 Copyright 2013 ScalaNLP
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/
package nak.core

import org.scalatest.FunSpec


/**
 * Test liblinear training on the prepositional phrase attachment code.
 */ 
class ClassifierSpec extends FunSpec {

  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear.LiblinearConfig
  import nak.util.ConfusionMatrix

  describe("Liblinear Training") {

    it ("should maintain accuracy for PPA data") {
      import nak.core.PrepattachUtil._

      val ppadir = "/data/ppa"

      // Get the training examples in their raw format.  
      val rawExamples = readRaw(ppadir+"/training").toList

      // Configure and train with liblinear. Here we use the (default) L2-Regularized 
      // Logistic Regression classifier with a C value of .5 and eps of .1.
      val config = LiblinearConfig(cost=.5,eps=.1)
      val classifier = trainClassifier(config, simpleFeaturizer, rawExamples)
      
      // Make predictions on the evaluation data. Because the classifier knows about
      // featurization, we can apply the classifier directly to each example using evalRaw.
      val comparisons = for (ex <- readRaw(ppadir+"/devset").toList) yield 
        (ex.label, classifier.predict(ex.features), ex.features)

      // Compute and print out the confusion matrix based on the comparisons 
      // obtained above.
      val (goldLabels, predictions, inputs) = comparisons.unzip3
      val cmatrix = ConfusionMatrix(goldLabels, predictions, inputs)
      assert(cmatrix.scores.accuracy == 0.8224808120821986)
    }
  }
}


object PrepattachUtil {

  import nak.data._

  // A function (with supporting regex) that reads the format of the PPA 
  // files and turns them into Examples. E.g. a line like:
  //   0 join board as director V
  // becames an Example with "V" as the label, and "join board as director"
  // as the features. Normally we'd go ahead and transform this into better
  // features, but this shows what you'd be more likely to do if reading in
  // documents.
  val PpaLineRE = """^(\d+)\s(.*)\s(N|V)$""".r

  def readRaw(resourceFilename: String) = {
    val rawStream = this.getClass.getResourceAsStream(resourceFilename)
    for (PpaLineRE(id,obs,label) <- io.Source.fromInputStream(rawStream).getLines)
    yield Example(label, obs)
  }

  // A featurizer that simply splits the raw inputs and attaches the
  // appropriate attributes to each of the elements.
  lazy val simpleFeaturizer = new Featurizer[String,String] {
    def apply(input: String) = {
      val attributes = Array("verb","object","prep","prep-obj")
      for ((attr,value) <- attributes.zip(input.split("\\s")))
      yield FeatureObservation(attr+"="+value)
    }
  }


}
