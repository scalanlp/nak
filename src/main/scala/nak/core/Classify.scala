package nak.core

trait Classifier extends (Array[(Int,Double)] => Array[Double])

trait FeatureMap[F] {
  def indexOfFeature(feature: F): Option[Int]
}

trait LabelMap[L] {
  def indexOfLabel(label: L): Int
  def labelOfIndex(index: Int): L
  def numLabels: Int
}

trait IndexedClassifier[L,F] extends Classifier with LabelMap[L] with FeatureMap[F]

trait StringIndexedClassifier extends IndexedClassifier[String,String]

trait LinearModelAdaptor extends nak.core.LinearModel with StringIndexedClassifier {

  def eval(observations: Array[(String,Double)]) = {
    val indexedValues = observations.flatMap { case(c,v) => indexOfFeature(c) match {
      case Some(index) => Some((index, v))
      case None => None
    }}
    apply(indexedValues)
  }

  def eval(context: Array[String], values: Array[Float]): Array[Double] = 
    eval(context.zip(values.map(_.toDouble)))

  def eval(context: Array[String]) = eval(context.zip(Stream.continually(1.0)))
    
  def getOutcome(i: Int) = labelOfIndex(i)
  def getIndex(outcome: String) = indexOfLabel(outcome)
  def getNumOutcomes = numLabels

}

object ClassifierUtil {

  import nak.core.LinearModel
  import java.text.DecimalFormat

  /**
   * Return the name of the outcome corresponding to the highest likelihood
   * in the parameter ocs.
   *
   * @param ocs A double[] as returned by the eval(String[] context)
   *            method.
   * @return    The name of the most likely outcome.
   */
  def getBestOutcome(model: LinearModel, ocs: Array[Double]) = {
    var best = 0
    for (i <- 1 until ocs.length)
      if (ocs(i) > ocs(best)) best = i
    model.getOutcome(best)
  }


  /**
   * Return a string matching all the outcome names with all the
   * probabilities produced by the <code>eval(String[] context)</code>
   * method.
   *
   * @param ocs A <code>double[]</code> as returned by the
   *            <code>eval(String[] context)</code>
   *            method.
   * @return    String containing outcome names paired with the normalized
   *            probability (contained in the <code>double[] ocs</code>)
   *            for each one.
   */
  def getAllOutcomes(model: LinearModel, ocs: Array[Double]) = {
      if (ocs.length != model.getNumOutcomes) {
          "The double array sent as a parameter to GISModel.getAllOutcomes() must not have been produced by this model."
      } else {
        val df =  new DecimalFormat("0.0000")
        val sb = new StringBuilder(ocs.length * 2)
        sb.append(model.getOutcome(0)).append("[").append(df.format(ocs(0))).append("]")
        for (i <- 1 until ocs.length)
          sb.append("  ")
            .append(model.getOutcome(i))
            .append("[").append(df.format(ocs(i))).append("]")
        sb.toString
      }
  }


}
