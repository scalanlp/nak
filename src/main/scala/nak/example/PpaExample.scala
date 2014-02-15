package nak.example

/*
 Copyright 2013 Jason Baldridge
 
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


/**
 * An example of using the API to classify the prepositional phrase attachment
 * data, trying to be as simple and self-contained as possible. Includes example
 * of how to serialize (save) and deserialize (load) a classifier.
 *
 * After compiling Nak, you can run it as follows (in the top-level of Nak):
 *
 * $ bin/nak run nak.example.PpaExample data/classify/ppa/training data/classify/ppa/devset ppa-classifier.obj 
 * 
 * @author jasonbaldridge
 */
object PpaExample {

  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear.LiblinearConfig
  import nak.util.ConfusionMatrix

  def main(args: Array[String]) {
    val Array(trainfile, evalfile, fileForSavedClassifier) = args

    // A function (with supporting regex) that reads the format of the PPA
    // files and turns them into Examples. E.g. a line like:
    //   0 join board as director V
    // becames an Example with "V" as the label, and "join board as director"
    // as the features. Normally we'd go ahead and transform this into better
    // features, but this shows what you'd be more likely to do if reading in
    // documents.
    val PpaLineRE = """^(\d+)\s(.*)\s(N|V)$""".r
    def readRaw(filename: String) =
      for (PpaLineRE(id,obs,label) <- io.Source.fromFile(filename).getLines)
      yield Example(label, obs)

    // A featurizer that simply splits the raw inputs and attaches the
    // appropriate attributes to each of the elements.
    val featurizer = new Featurizer[String,String] {
      def apply(input: String) = {
        val attributes = Array("verb","object","prep","prep-obj")
        for ((attr,value) <- attributes.zip(input.split("\\s")))
        yield FeatureObservation(attr+"="+value)
      }
    }

    // Get the training examples in their raw format.
    val rawExamples = readRaw(trainfile).toList
    
    // Configure and train with liblinear. Here we use the (default) L2-Regularized
    // Logistic Regression classifier with a C value of .5. We accept the default
    // eps and verbosity values.
    val config = LiblinearConfig(cost=.5)
    val trainedClassifier = trainClassifier(config, featurizer, rawExamples)
    
    // Save the classifier to disk, then load it. Obviously not necessary here, but this
    // shows how to do it.
    saveClassifier(trainedClassifier, fileForSavedClassifier)
    val classifier = loadClassifier[FeaturizedClassifier[String,String]](fileForSavedClassifier)

    // Make predictions on the evaluation data. Because the classifier knows about
    // featurization, we can apply the classifier directly to each example using evalRaw.
    val comparisons = for (ex <- readRaw(evalfile).toList) yield
      (ex.label, classifier.predict(ex.features), ex.features)

    // Compute and print out the confusion matrix based on the comparisons
    // obtained above.
    val (goldLabels, predictions, inputs) = comparisons.unzip3
    println(ConfusionMatrix(goldLabels, predictions, inputs))
  }

}

object PpaExampleBinomial {

  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear.LiblinearConfig
  import nak.util.ConfusionMatrix

  def main(args: Array[String]) {
    val Array(trainfile, evalfile, fileForSavedClassifier) = args

    // A function (with supporting regex) that reads the format of the PPA
    // files and turns them into Examples. E.g. a line like:
    //   0 join board as director V
    // becames an Example with "V" indicating "success" as its label ("N"
    // is "failure"), and "join board as director"
    // as the features. Normally we'd go ahead and transform this into better
    // features, but this shows what you'd be more likely to do if reading in
    // documents.
    val PpaLineRE = """^(\d+)\s(.*)\s(N|V)$""".r
    def readRaw(filename: String) =
      for (PpaLineRE(id,obs,label) <- io.Source.fromFile(filename).getLines)
      yield {
        val successFail = if (label=="V") (1,0) else (0,1)
        Example(successFail, obs)
      }

    // A featurizer that simply splits the raw inputs and attaches the
    // appropriate attributes to each of the elements.
    val featurizer = new Featurizer[String,String] {
      def apply(input: String) = {
        val attributes = Array("verb","object","prep","prep-obj")
        for ((attr,value) <- attributes.zip(input.split("\\s")))
        yield FeatureObservation(attr+"="+value)
      }
    }

    // Get the training examples in their raw format.
    val rawExamples = readRaw(trainfile).toList
    
    // Configure and train with liblinear. Here we use the (default) L2-Regularized
    // Logistic Regression classifier with a C value of .5. We accept the default
    // eps and verbosity values.
    val config = LiblinearConfig(cost=0.5)
    val trainedClassifier = trainBinomialClassifier(config, featurizer, rawExamples)
    
    // Save the classifier to disk, then load it. Obviously not necessary here, but this
    // shows how to do it.
    saveClassifier(trainedClassifier, fileForSavedClassifier)
    val classifier = loadClassifier[FeaturizedClassifier[String,String]](fileForSavedClassifier)

    // Make predictions on the evaluation data. Because the classifier knows about
    // featurization, we can apply the classifier directly to each example using evalRaw.
    val comparisons = for (ex <- readRaw(evalfile).toList) yield {
      val goldLabel = if (ex.label._1 == 1) "Y" else "N"
      (goldLabel, classifier.predict(ex.features), ex.features)
    }

    // Compute and print out the confusion matrix based on the comparisons
    // obtained above.
    val (goldLabels, predictions, inputs) = comparisons.unzip3
    println(ConfusionMatrix(goldLabels, predictions, inputs))
  }

}
