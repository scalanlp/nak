package nak.core

import nak.liblinear.{Model => LiblinearModel, FeatureNode, LiblinearConfig, Linear, Problem, Parameter}
import nak.data.{Example, FeatureObservation}

trait FeatureMap {
  def indexOfFeature(feature: String): Option[Int]
}

trait LabelMap[L] {
  def indexOfLabel(label: L): Int
  def labelOfIndex(index: Int): L
  def numLabels: Int
}


trait Classifier extends (Array[(Int,Double)] => Array[Double])

trait IndexedClassifier[L] extends Classifier with LabelMap[L] with FeatureMap

trait StringIndexedClassifier extends IndexedClassifier[String]

trait LinearModelAdaptor extends nak.core.LinearModel with StringIndexedClassifier {

  def evalIndexed(observations: Seq[FeatureObservation[Int]]): Array[Double] =
    apply(observations.map(_.tuple).toArray)

  def evalRaw(observations: Seq[FeatureObservation[String]]): Array[Double] =
    evalIndexed(observations.flatMap(_.mapOption(indexOfFeature)))

  def eval(context: Array[String], values: Array[Float]): Array[Double] = {
    val fobservations = 
      for ((f,m) <- context.zip(values);
           fob = FeatureObservation(f,m.toDouble);
           indexed <- fob.mapOption(indexOfFeature)) yield indexed.tuple
    apply(fobservations)
  }
  
  def eval(context: Array[String]) = 
    eval(context, Array.fill(context.length)(1.0f))
    
  def getOutcome(i: Int) = labelOfIndex(i)
  def getIndex(outcome: String) = indexOfLabel(outcome)
  def getNumOutcomes = numLabels

}

object ClassifierUtil {

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


/**
 * A wrapper to Liblinear that conforms to the LinearModel interface.
 * 
 * @author jasonbaldridge
 */
class LiblinearClassifier(
  val model: LiblinearModel, 
  val lmap: Map[String, Int], 
  val fmap: Map[String, Int]) extends LinearModelAdaptor {

  import nak.liblinear.{Feature => LiblinearFeature}

  // Assumes labels are indexed 0 to n in label map
  val labels = lmap.toSeq.sortBy(_._2).unzip._1.toArray

  def indexOfFeature(feature: String) = fmap.get(feature)
  def indexOfLabel(label: String) = lmap(label)
  def labelOfIndex(index: Int) = labels(index)
  val numLabels = labels.length

  val getModelType = nak.core.AbstractModel.ModelType.Liblinear

  def apply(context: Array[(Int,Double)]): Array[Double] =
    predict(context.map(c=>new FeatureNode(c._1,c._2).asInstanceOf[LiblinearFeature]))

  def predict(context: Array[LiblinearFeature]): Array[Double] = {
    val labelScores = Array.fill(numLabels)(0.0)
    Linear.predictProbability(model, context, labelScores)
    labelScores
  }
}

object LiblinearClassifier {
 
  def apply(model: LiblinearModel, labels: Array[String], features: Array[String]) =
    new LiblinearClassifier(model, labels.zipWithIndex.toMap, features.zipWithIndex.toMap)

}

/**
 * Train a Liblinear classifier from data.
 * 
 * @author jasonbaldridge
 */
class LiblinearTrainer(config: LiblinearConfig) {

  import nak.liblinear.{Feature => LiblinearFeature}
  import nak.data.Example
  import LiblinearTrainer._

  if (!config.showDebug) Linear.disableDebugOutput

  def apply(
    responses: Seq[Double],
    observations: Seq[Seq[(Int,Double)]], 
    numFeatures: Int
  ): LiblinearModel =
    apply(responses.toArray, createLiblinearMatrix(observations), numFeatures)


  def apply(
    responses: Array[Double],
    observations: Array[Array[LiblinearFeature]],
    numFeatures: Int
  ): LiblinearModel = {

    val problem = new Problem
    problem.y = responses
    problem.x = observations
    problem.l = responses.length
    problem.n = numFeatures

    // Can make the solver type a parameter if want to use other solvers in LibLinear.
    val param = new Parameter(config.solverType, config.cost, config.eps)
    Linear.train(problem, param)
  }

}

/**
 * A helper object
 */
object LiblinearTrainer {

  import nak.liblinear.{Feature => LiblinearFeature}

  def train(
    indexer: nak.data.DataIndexer, 
    config: LiblinearConfig = new LiblinearConfig()): LiblinearClassifier = {

    val labels = indexer.getOutcomeLabels
    
    // We unfortunately need to fix up the contexts so that feature indices start at 1.
    val zeroBasedFeatures = indexer.getPredLabels
    val zeroReindex = zeroBasedFeatures.length
    val features = zeroBasedFeatures ++ Array(zeroBasedFeatures(0))
    features(0) = "DUMMY FEATURE"
    
    val contexts = indexer.getContexts.map { context => {
      context.map { _ match {
        case 0 => zeroReindex
        case x => x
      }}
    }}

    // Use values of 1.0 if the features were binary.
    val values = 
      if (indexer.getValues != null) indexer.getValues
      else contexts.map(_.map(x=>1.0f))

    // Get the responses and observations ready for Liblinear
    val responses = indexer.getOutcomeList.map(_.toDouble)
    val observationsAsTuples = contexts.zip(values).map{ 
      case (c,v) => 
        c.zip(v).groupBy(_._1).mapValues(_.map(_._2)).mapValues(_.sum.toFloat).toArray
    }

    train(responses, observationsAsTuples, labels, features, config)
  }

  def train(
    responses: Array[Double],
    observationsAsTuples: Array[Array[(Int, Float)]],
    labels: Array[String],
    features: Array[String], 
    config: LiblinearConfig): LiblinearClassifier = {
    
    val observations = createLiblinearMatrix(observationsAsTuples)
    // Train the model, and then return the classifier.
    val model = new LiblinearTrainer(config)(responses, observations, features.length)
    LiblinearClassifier(model, labels, features);
  }

  def train(
    examples: TraversableOnce[Example[Int,Seq[FeatureObservation[Int]]]],
    lmap: Map[String, Int], 
    fmap: Map[String, Int],
    config: LiblinearConfig): LiblinearClassifier = {
  
    val (responses, observationsAsTuples) = 
      examples.map(ex => (ex.label, ex.features.map(_.tuple).toSeq)).toSeq.unzip
    
    val observations = createLiblinearMatrix(observationsAsTuples)
    // Train the model, and then return the classifier.
    val model = new LiblinearTrainer(config)(responses.map(_.toDouble).toArray, observations, fmap.size)
    new LiblinearClassifier(model, lmap, fmap)
  }


  def createLiblinearMatrix(observations: Seq[Seq[(Int,Double)]]): Array[Array[LiblinearFeature]] =  
    observations.map { features =>
      features.map{ case(a,v) => new FeatureNode(a,v).asInstanceOf[LiblinearFeature] }.toArray
    }.toArray

  def createLiblinearMatrix(observations: Array[Array[(Int,Float)]]): Array[Array[LiblinearFeature]] =  
    observations.map { features => {
      features
        .sortBy(_._1)
        .map{ case(a,v) => new FeatureNode(a,v).asInstanceOf[LiblinearFeature] }
    }}

}
