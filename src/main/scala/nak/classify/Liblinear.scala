package nak.classify

import de.bwaldvogel.liblinear._

class LiblinearClassifier(
  model: Model,
  fmap: Map[String,Int], 
  lmap: Map[String,Int]) extends LinearModelAdaptor {

  // Assumes labels are indexed 0 to n in label map
  val labels = lmap.toSeq.sortBy(_._2).unzip._1.toArray

  def indexOfFeature(feature: String) = fmap.get(feature)
  def indexOfLabel(label: String) = lmap(label)
  def labelOfIndex(index: Int) = labels(index)
  val numLabels = labels.length

  def apply(context: Array[(Int,Double)]): Array[Double] =
    predict(context.map(c=>new FeatureNode(c._1,c._2).asInstanceOf[Feature]))

  def predict(context: Array[Feature]): Array[Double] = {
    val labelScores = Array.fill(numLabels)(0.0)
    Linear.predictValues(model, context, labelScores)
    labelScores
  }
}


/**
 * A simple wrapper to Liblinear.
 * 
 * @author jasonbaldridge
 */
class LiblinearClassifierWrapper(model: Model) {
  lazy val weights = model.getFeatureWeights
  lazy val numClasses = model.getNrClass

  import LiblinearUtil.createNodes

  def apply(nodes: Seq[FeatureNode]) = predict(nodes)

  def predict(nodes: Seq[FeatureNode]) = 
    Linear.predict(model, nodes.toArray)

  def predictDense(inputValues: Seq[Double]) = 
    apply(createNodes(inputValues))

  def predictValues(nodes: Seq[FeatureNode]) = {
    val linearPredictor = Array.fill(numClasses)(0.0)
    Linear.predictValues(model, nodes.toArray, linearPredictor)
    linearPredictor.toSeq
  }

}

/**
 * Companion object to train a Liblinear classifier from data.
 * 
 * @author jasonbaldridge
 */
class LiblinearTrainer(
  regularization: Double = 1.0, 
  eps: Double = 0.01,
  showDebug: Boolean = false
) {

  import LiblinearUtil._

  if (!showDebug) Linear.disableDebugOutput

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
    val param = new Parameter(SolverType.L2R_LR, regularization, eps)
    val model = Linear.train(problem, param)
    //new LiblinearClassifierWrapper(model)
    model
  }

}
/**
 * Some convenience methods.
 * 
 * @author jasonbaldridge
 */
object LiblinearUtil {

  def createLiblinearMatrix(observations: Seq[Seq[(Int,Double)]]): Array[Array[Feature]] =  
    observations.map { features =>
      features.map{ case(a,v) => new FeatureNode(a,v).asInstanceOf[Feature] }.toArray
    }.toArray

  /**
   * Converts a 0-based matrix row into an Array of 1-based features for
   * Liblinear.
   */
  def createNodes(inputValues: Seq[Double]) = 
    inputValues
      .zipWithIndex
      .map { case(v, i) => new FeatureNode(i+1,v) }
      .toArray
  
}
