package nak.classify

import de.bwaldvogel.liblinear._

/**
 * A simple wrapper to Liblinear.
 * 
 * @author jasonbaldridge
 */
class LiblinearClassifier(model: Model) {
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

  def applyIndexed(
    responses: Seq[Double],
    designMatrix: Seq[Seq[(Int,Double)]], 
    numFeatures: Int
  ) =
    apply(responses.toArray, createLiblinearMatrix(designMatrix), numFeatures)


  def apply(
    responses: Array[Double],
    designMatrix: Array[Array[Feature]],
    numFeatures: Int
  ) = {

    val problem = new Problem
    problem.y = responses
    problem.x = designMatrix
    problem.l = responses.length
    problem.n = numFeatures
 
    // Can make the solver type a parameter if want to use other solvers in LibLinear.
    val param = new Parameter(SolverType.L2R_LR, regularization, eps)
    val model = Linear.train(problem, param)
    new LiblinearClassifier(model)
  }

}
/**
 * Some convenience methods.
 * 
 * @author jasonbaldridge
 */
object LiblinearUtil {

  def createLiblinearMatrix(designMatrix: Seq[Seq[(Int,Double)]]): Array[Array[Feature]] =  
    designMatrix.map { features =>
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
