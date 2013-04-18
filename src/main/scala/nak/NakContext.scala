package nak

import java.io.{BufferedReader,FileReader}

import nak.core._
import nak.data._

import nak.liblinear.{Model => LiblinearModel, LiblinearConfig}
import nak.liblinear.LiblinearUtil._

/**
 * An object that provides common functions needed for using Nak.
 *
 * @author jasonbaldridge
 */
object NakContext {

  /**
   * Convert examples stored in CSV format (one per line) into a sequence of Examples.
   */
  def fromCsvFile(filename: String): Iterator[Example[String,Seq[FeatureObservation[String]]]] = {
    for (line <- scala.io.Source.fromFile(filename).getLines) yield {
      val items = line.split(",")
      val features = items.dropRight(1).map(descr=>FeatureObservation(descr))
      val label = items.last
      Example(label, features)
    }
  }

  /**
   * Given a sequence of feature observations (a feature and its magnitude), combine
   * multiple instances of the same feature, and then sort the result.
   *
   * E.g. Seq[("foo",1.0),("bar",1.0),("foo",2.0)]
   *  becomes
   *      Seq[("bar",1.0),("foo",3.0)]
   */
  def condense(features: Seq[FeatureObservation[Int]]) =
    features
      .groupBy(_.feature)
      .values
      .map(_.reduce(_+_))
      .toSeq
      .sortBy(_.feature)


  /**
   * Given the labels and scores that have been produced for each, return the label
   * with the highest score.
   */
  def maxLabel(labels: Seq[String])(scores: Seq[Double]) =
    labels.zip(scores).maxBy(_._2)._1


  /**
   * Trains a classifier given examples and featurizer. Handles indexation of features,
   * and creates a classifier that can be applied directly to new raw observations.
   *
   * This is the easiest way to build and use a classifier.
   */
  def trainClassifier[I](
    config: LiblinearConfig, 
    featurizer: Featurizer[I,String], 
    rawExamples: Seq[Example[String, I]]
  ): IndexedClassifier[String] with FeaturizedClassifier[String, I] = {

    // Featurize and index the examples.
    val indexer = new ExampleIndexer    
    val examples = rawExamples.map(_.map(featurizer)).map(indexer)
    val (lmap,fmap) = indexer.getMaps
        
    // Train the model, and then return the classifier.
    val model = trainModel(config, examples, fmap.size)
    Classifier(model, lmap, fmap, featurizer)
  }

  /**
   * Trains a classifier given indexed examples and the label and feature maps produced
   * by indexation.
   */ 
  def trainClassifier(
    config: LiblinearConfig,
    examples: TraversableOnce[Example[Int,Seq[FeatureObservation[Int]]]],
    lmap: Map[String, Int], 
    fmap: Map[String, Int]
  ): IndexedClassifier[String] = 
    Classifier(trainModel(config,examples,fmap.size), lmap, fmap)

  /**
   * Trains a liblinear model given indexed examples. Note: a model is basically just the
   * parameters. The classifiers returned by trainClassifier methods wrap the parameters
   * into a number of convenience methods that makes it easier to use the model.
   */
  def trainModel(
    config: LiblinearConfig,
    examples: TraversableOnce[Example[Int,Seq[FeatureObservation[Int]]]],
    numFeatures: Int): LiblinearModel = {

    val (responses, observationsAsTuples) = 
      examples.map(ex => (ex.label, ex.features.map(_.tuple).toSeq)).toSeq.unzip
    val observations = createLiblinearMatrix(observationsAsTuples)
    new LiblinearTrainer(config)(responses.map(_.toDouble).toArray, observations, numFeatures)
  }


}
