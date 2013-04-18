package nak

import java.io.{BufferedReader,FileReader}

import nak.data.{Example, FeatureObservation}

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


}
