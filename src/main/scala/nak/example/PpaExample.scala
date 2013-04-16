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


import nak.core._
import nak.data._
import nak.liblinear.{LiblinearConfig,Solver}
import nak.util.ConfusionMatrix

/**
 * An example of using the API to classify the prepositional phrase attachment
 * data.
 *
 * @author jasonbaldridge
 */
object PpaExample {

  def main(args: Array[String]) {
    val Array(trainfile,evalfile) = args

    // Read and index the examples in the training file.
    val PpaLineRE = """^(\d+)\s(.*)\s(N|V)$""".r

    // A function that reads the format of the PPA files and turns them into
    // examples. E.g. a line like:
    //   0 join board as director V
    // becames an Example with "V" as the label, and "join board as director"
    // as the features. Normally we'd go ahead and transform this into better
    // features, but this shows what you'd be more likely to do if reading in
    // documents.
    def readNative(filename: String) = 
      for (PpaLineRE(id,obs,label) <- io.Source.fromFile(filename).getLines)
        yield Example(label, obs)

    // Get the training examples in their native format.  
    val nativeExamples = readNative(trainfile).toList
    println("Native example: " + nativeExamples.head)

    // A featurizer that simply splits the native inputs and attaches the
    // appropriate attributes to each of the elements.
    val featurizer = new Featurizer[String,String] {
      def apply(input: String) = {
        val attributes = Array("verb","object","prep","prep-obj")
        val values = input.split("\\s")
        for ((a,v) <- attributes.zip(values)) 
          yield FeatureObservation(a+"="+v)
      }
    }

    // Now apply the featurizer to the examples. We are mapping over a sequence
    // of Examples, and for each Example, we map the featurizer over it. The latter
    // simply applies the featurizer to the 'features' value in the Example.
    val featurizedExamples = nativeExamples.map(ex => ex.map(featurizer))
    println("Featurized example: " + featurizedExamples.head)

    // The features were extracted above, but they are still represented as
    // Strings. Liblinear needs these to be numerical indices, and the 
    // ExampleIndexer will take care of this for us and give us the resulting
    // mapping from labels to indices and features to indices.
    val indexer = new ExampleIndexer
    val indexedExamples = featurizedExamples.map(indexer)
    val (lmap,fmap) = indexer.getMaps
    println("Indexed example: " + indexedExamples.head)

    // Configure and train with liblinear. Here we use the L2-Regularized 
    // Logistic Regression classifier with a C value of .5. We accept the default
    // eps and verbosity values.
    val config = new LiblinearConfig(Solver("L2R_LR"), .5)
    val classifier = LiblinearTrainer.train(indexedExamples, lmap, fmap, config)

    // Make predictions on the evaluation data. 
    val nativeEvalExamples = readNative(evalfile).toList
    val comparisons = for (ex <- nativeEvalExamples) yield {

      // Because the classifier knows about indexation, we only need to extract 
      // the features from each example and then use evalRaw to that.
      val scores = classifier.evalRaw(featurizer(ex.features))

      // Get the *index* of the best score.
      val best = scores.zipWithIndex.maxBy(_._1)._2

      // Output both the true label and the predicted label.
      (ex.label, classifier.labelOfIndex(best))
    }

    // Compute and print out the confusion matrix based on the comparisons 
    // obtained above.
    val (goldLabels, predictions) = comparisons.unzip
    val inputs = nativeEvalExamples.map(_.features)
    val cmatrix = ConfusionMatrix(goldLabels, predictions, inputs)
    println(cmatrix)
  }

}
