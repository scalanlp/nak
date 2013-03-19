package nak.liblinear

import nak.core._


/**
 * A wrapper to Liblinear that conforms to the LinearModel interface.
 * 
 * @author jasonbaldridge
 */
class LiblinearClassifier(
  val model: Model, 
  val lmap: Map[String, Int], 
  val fmap: Map[String, Int]) extends LinearModelAdaptor {

  // Assumes labels are indexed 0 to n in label map
  val labels = lmap.toSeq.sortBy(_._2).unzip._1.toArray

  def indexOfFeature(feature: String) = fmap.get(feature)
  def indexOfLabel(label: String) = lmap(label)
  def labelOfIndex(index: Int) = labels(index)
  val numLabels = labels.length

  val getModelType = nak.core.AbstractModel.ModelType.Liblinear

  def apply(context: Array[(Int,Double)]): Array[Double] =
    predict(context.map(c=>new FeatureNode(c._1,c._2).asInstanceOf[Feature]))

  def predict(context: Array[Feature]): Array[Double] = {
    val labelScores = Array.fill(numLabels)(0.0)
    Linear.predictProbability(model, context, labelScores)
    labelScores
  }
}

object LiblinearClassifier {
  
  def apply(model: Model, labels: Array[String], features: Array[String]) =
    new LiblinearClassifier(model, labels.zipWithIndex.toMap, features.zipWithIndex.toMap)

}


class LiblinearConfig(
  val solverType: SolverType = SolverType.L2R_LR,
  val cost: Double = 1.0, 
  val eps: Double = 0.01, 
  val showDebug: Boolean = false)

/**
 * Train a Liblinear classifier from data.
 * 
 * @author jasonbaldridge
 */
class LiblinearTrainer(config: LiblinearConfig) {

  import LiblinearTrainer._

  if (!config.showDebug) Linear.disableDebugOutput

  def apply(
    responses: Seq[Double],
    observations: Seq[Seq[(Int,Double)]], 
    numFeatures: Int
  ): Model =
    apply(responses.toArray, createLiblinearMatrix(observations), numFeatures)


  def apply(
    responses: Array[Double],
    observations: Array[Array[Feature]],
    numFeatures: Int
  ): Model = {

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

  def train(indexer: nak.data.DataIndexer, config: LiblinearConfig = new LiblinearConfig()) = {

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
    
    val observations = createLiblinearMatrix(observationsAsTuples)
    //val observations = createLiblinearMatrix(observationsAsTuples)
    //  .zip(indexer.getNumTimesEventsSeen)
    //  .map { case(event, count) => Stream.continually(event).take(count).toArray }
    //  .flatten

    // Train the model, and then return the classifier.
    val model = new LiblinearTrainer(config)(responses, observations, features.length)

    LiblinearClassifier(model, labels, features);
  }

  def createLiblinearMatrix(observations: Seq[Seq[(Int,Double)]]): Array[Array[Feature]] =  
    observations.map { features =>
      features.map{ case(a,v) => new FeatureNode(a,v).asInstanceOf[Feature] }.toArray
    }.toArray

  def createLiblinearMatrix(observations: Array[Array[(Int,Float)]]): Array[Array[Feature]] =  
    observations.map { features => {
      features
        .sortBy(_._1)
        .map{ case(a,v) => new FeatureNode(a,v).asInstanceOf[Feature] }
    }}

}
