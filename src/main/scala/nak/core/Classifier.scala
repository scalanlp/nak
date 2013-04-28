package nak.core

/**
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

import nak.data._
import nak.liblinear.{Model => LiblinearModel, LiblinearConfig}
import nak.liblinear.LiblinearUtil._

/**
 * Classifiers are given a sequence of feature indices and their magnitudes
 * and return a score for each label.
 */
trait Classifier extends (Array[(Int,Double)] => Array[Double]) {

  /**
   * Evaluate the given indexed feature observations. Calls the apply method,
   * which must interface with some underlying representation of the system
   * used; e.g. see the LiblinearClassifier class.
   */ 
  def evalIndexed(observations: Seq[FeatureObservation[Int]]): Array[Double] =
    apply(observations.map(_.tuple).toArray)

}

/**
 * A classifier that knows the actual descriptions of the labels and features,
 * rather than just their indices.
 */
trait IndexedClassifier[L] extends Classifier with LabelMap[L] with FeatureMap {

  /**
   * Evaluate the un-indexed feature observations. Uses the feature map to
   * index the observations and then pass them on to evalIndexed of Classifier.
   */ 
  def evalUnindexed(observations: Seq[FeatureObservation[String]]): Array[Double] =
    evalIndexed(observations.flatMap(_.mapOption(indexOfFeature)))

}

/**
 * A classifier that has a featurizer that allows it to be applied directly to
 * raw inputs.
 */
trait FeaturizedClassifier[L,I] extends IndexedClassifier[L] {
  val featurizer: Featurizer[I,String]

  /**
   * Evaluate a raw observation by featurizing it and then calling evalUnindexed
   * of IndexedClassifier.
   */ 
  def evalRaw(content: I) = evalUnindexed(featurizer(content))


  /**
   * Evaluate a raw observation by using evalRaw, identify the highest
   * scoring label, and then return it.
   */ 
  def predict(content: I) = 
    labelOfIndex(evalRaw(content).zipWithIndex.maxBy(_._1)._2)

}


/**
 * A classifier that wraps a liblinear model and conforms to the Nak API.
 * 
 * @author jasonbaldridge
 */
trait LiblinearClassifier extends IndexedClassifier[String] {
  val model: LiblinearModel 
  val lmap: Map[String, Int] 
  val fmap: Map[String, Int]

  import nak.liblinear.{Linear, FeatureNode, Feature => LiblinearFeature}

  // Assumes labels are indexed 0 to n in label map
  lazy val labels = lmap.toSeq.sortBy(_._2).unzip._1
  lazy val numLabels = labels.length

  /** Get the index of a label. */ 
  def indexOfLabel(label: String) = lmap(label)

  /** Get the label at the given index. */ 
  def labelOfIndex(index: Int) = labels(index)

  /** Get the index of a feature. */ 
  def indexOfFeature(feature: String) = fmap.get(feature)

  /**
   * Declare the model type. Used for legacy model saving; will be phased out soon.
   */ 
  val getModelType = nak.core.AbstractModel.ModelType.Liblinear

  /**
   * Implement the apply method of Classifier by transforming the tuples into
   * Liblinear Features and then calling Linear.predictProbability.
   *
   * TODO: This should be made more general so that the SVM solvers can be used
   * by Nak.
   */ 
  def apply(context: Array[(Int,Double)]): Array[Double] = {
    val ctxt = context.map(c=>new FeatureNode(c._1,c._2).asInstanceOf[LiblinearFeature])
    val labelScores = Array.fill(numLabels)(0.0)
    Linear.predictProbability(model, ctxt, labelScores)
    labelScores
  }

}

/**
 * An adaptor that makes the new Nak API classifiers conform to the legacy
 * LinearModel that came from OpenNLP.
 */
@deprecated(message="Allows liblinear classifiers to implement the legacy API, but this be phased out soon.",since="1.1.2")
trait LinearModelAdaptor extends LiblinearClassifier with nak.core.LinearModel {

  def eval(context: Array[String], values: Array[Float]): Array[Double] = {
    val fobservations = 
      for ((f,m) <- context.zip(values);
           fob = FeatureObservation(f,m.toDouble);
           indexed <- fob.mapOption(indexOfFeature)) yield indexed.tuple
    apply(fobservations)
  }
  
  def eval(context: Array[String]) = 
    evalUnindexed(context.map(FeatureObservation(_)))
    
  def getOutcome(i: Int) = labelOfIndex(i)
  def getIndex(outcome: String) = indexOfLabel(outcome)
  lazy val getNumOutcomes = numLabels

}



/**
 * Companion object to help with constructing Classifiers.
 */
object Classifier {

  /**
   * Create an IndexedClassifier given a model and arrays for labels and
   * features (where the index of feature in the array is the index of the
   * parameter in the model).
   */ 
  def apply(_model: LiblinearModel, 
            labels: Array[String], 
            features: Array[String]): IndexedClassifier[String] =
    apply(_model, labels.zipWithIndex.toMap, features.zipWithIndex.toMap)


  /**
   * Create an IndexedClassifier given a model and arrays for labels and
   * features (where the index of feature in the array is the index of the
   * parameter in the model).
   */ 
  def apply(_model: LiblinearModel, _lmap: Map[String,Int], _fmap: Map[String,Int]) =
    new LiblinearClassifier {
      val model = _model
      val lmap = _lmap
      val fmap = _fmap
    }

  /**
   * Create an classifier that is indexed and contains a featurizer, given a model,
   * a featurizer, and maps giving the indices of the labels and features (where the
   * index of feature in the array is the index of the parameter in the model).
   */ 
  def apply[I](_model: LiblinearModel, 
               _lmap: Map[String,Int],
               _fmap: Map[String,Int],
               _featurizer: Featurizer[I,String]) =
    new LiblinearClassifier with FeaturizedClassifier[String,I] {
      val model = _model
      val lmap = _lmap
      val fmap = _fmap
      val featurizer = _featurizer
    }

  /**
   * Create an LinearModelAdaptor given a model and arrays for labels and
   * features (where the index of feature in the array is the index of the
   * parameter in the model). (LinearModelAdaptor implements the LinearModel
   * interface from the legacy OpenNLP API.)
   */ 
  @deprecated(message="Use new API classifiers instead.", since="1.1.2")
  def createLegacy(_model: LiblinearModel, _labels: Array[String], _features: Array[String]) =
    new LinearModelAdaptor { 
      val model = _model
      val lmap = _labels.zipWithIndex.toMap
      val fmap = _features.zipWithIndex.toMap
    }

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
  @deprecated(message="Use Classifier rather than LinearModel.", since="1.1.2")
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
  @deprecated(message="Use Classifier rather than LinearModel.", since="1.1.2")
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
