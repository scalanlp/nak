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
trait Classifier extends (Array[(Int,Double)] => Array[Double]) with Serializable {

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
  def fmap: FeatureMap

  import nak.liblinear.{Linear, FeatureNode, Feature => LiblinearFeature}

  // Assumes labels are indexed 0 to n in label map
  lazy val labels = lmap.toSeq.sortBy(_._2).unzip._1
  lazy val numLabels = labels.length

  /** Get the index of a label. */ 
  def indexOfLabel(label: String) = lmap(label)

  /** Get the label at the given index. */ 
  def labelOfIndex(index: Int) = labels(index)

  /** Get the index of a feature. */ 
  def indexOfFeature(feature: String) = fmap.indexOfFeature(feature)

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
      val fmap = new ExactFeatureMap(_fmap)
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
      val fmap = new ExactFeatureMap(_fmap)
      val featurizer = _featurizer
    }

  /**
   * Create an classifier that is indexed and contains a featurizer, given a model,
   * a featurizer, and maps giving the indices of the labels and features (where the
   * index of feature in the array is the index of the parameter in the model).
   */ 
  def apply[I](_model: LiblinearModel, 
               _lmap: Map[String,Int],
               _fmap: HashedFeatureMap,
               _featurizer: Featurizer[I,String]) =
    new LiblinearClassifier with FeaturizedClassifier[String,I] {
      val model = _model
      val lmap = _lmap
      val fmap = _fmap
      val featurizer = _featurizer
    }
}
