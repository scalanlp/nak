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
package nak.example

/**
 * An example of using the API to train and evaluate a classifier using the
 * well-known 20 news groups data.
 *
 * After compiling Nak, you can run it as follows (in the top-level of Nak):
 *
 * - Obtain the corpus: http://qwone.com/~jason/20Newsgroups/20news-bydate.tar.gz
 * - Unpack the archive somewhere.
 *
 * $ nak run nak.example.TwentyNewsGroupsExample /path/to/where/you/put/20news
 * 
 */
object TwentyNewsGroupsExample {

  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear.LiblinearConfig
  import nak.util.ConfusionMatrix

  import java.io.File

  def main(args: Array[String]) {
    val newsgroupsDir = new File(args(0))

    // We need this codec for reading in the 20 news groups files.
    implicit val isoCodec = scala.io.Codec("ISO-8859-1")

    // Example stopword set (you should use a more extensive list for actual classifiers).
    val stopwords = Set("the","a","an","of","in","for","by","on")

    // Train
    print("Training... ")
    val trainDir = new File(newsgroupsDir, "20news-bydate-train")
    val trainingExamples = fromLabeledDirs(trainDir).toList
    val config = LiblinearConfig(cost=5.0,eps=0.01)
    val featurizer = new BowFeaturizer(stopwords)
    val classifier = trainClassifier(config, featurizer, trainingExamples)

    // Comment out the above line and uncomment the following if you want to try
    // the hashing trick.
    //val classifier = trainClassifierHashed(config, featurizer, trainingExamples, 50000)

    println("done.")
    
    // Evaluate
    println("Evaluating...")
    val evalDir = new File(newsgroupsDir, "20news-bydate-test")
    val comparisons = for (ex <- fromLabeledDirs(evalDir).toList) yield
      (ex.label, classifier.predict(ex.features), ex.features)
    val (goldLabels, predictions, inputs) = comparisons.unzip3
    println(ConfusionMatrix(goldLabels, predictions, inputs))
  }
  
}

object TwentyNewsGroupsKmeansExample {

  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.cluster._
  import nak.util.ConfusionMatrix
  import breeze.linalg.SparseVector
  
  import java.io.File

  def main(args: Array[String]) {
    val newsgroupsDir = new File(args(0))

    // We need this codec for reading in the 20 news groups files.
    implicit val isoCodec = scala.io.Codec("ISO-8859-1")

    // Example stopword set (you should use a more extensive list for actual classifiers).
    val stopwords = Set("the","a","an","of","in","for","by","on")

    // Train
    print("Training... ")
    val trainDir = new File(newsgroupsDir, "20news-bydate-train")
    val trainingExamples = fromLabeledDirs(trainDir).toList
    val featurizer = new BowFeaturizer(stopwords)
    val indexer = new ExampleIndexer
    val examples = trainingExamples.map(_.map(featurizer)).map(indexer)
    val (lmap,fmap) = indexer.getMaps
    val numFeatures = fmap.size
    val sparseExamples = examples.toIndexedSeq.map { example =>
      example.map { features =>
        SparseVector(numFeatures)(condense(features).map(_.tuple): _*)
      }.features
    }
    val kmeans = new Kmeans[SparseVector[Double]](sparseExamples,0.1,5)
    val (dispersion, centroids) = kmeans.run(20,1)
    println(s"Dispersion: $dispersion")
    println("Centroids:")
    centroids.foreach(c=>println(c(0 until 5).toArray.mkString(" ")))

    println("Evaluating...")
    val goldLabels = examples.map(_.label)
    val (distance, predictions) = kmeans.computeClusterMemberships(centroids)
    val inputs = examples.map(_.features)
    println(ConfusionMatrix(goldLabels, predictions, inputs))
    println(lmap.toSeq.foreach(println))
  }
  
}
