package nak.data

/*
 Copyright 2009 David Hall, Daniel Ramage
 
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

/**
 * Represents a single example from a collection of data. Intentionally overly general.
 *
 * @author dlwh
 */
trait Example[+L,+T] extends Observation[T] with Labeled[L] with Serializable {outer=>
  def id : String
  def label: L

  /** 
   * Converts the features in this example to a different one while still
   * preserving label and id. 
   */
  override def map[U](f: T=>U):Example[L, U] = new Example[L,U] {
    val label = outer.label
    val id = outer.id
    val features = f(outer.features)
  }

  /** 
   * Converts the label in this example to a different one while still
   * preserving features and id. 
   */
  def relabel[L2](f: L=>L2):Example[L2, T] = new Example[L2,T] {
    val label = f(outer.label)
    val id = outer.id
    val features = outer.features
  }


  /** 
   * Converts the features in this example to a different one while still
   * preserving label and id. 
   */
  override def flatMap[U](f: T=>U) = map(f)

  override def toString = {
    "Example { id =" + id + ", label = " + label + ", features = " + features + "}"
  }

}

object Example {

  /**
   * Create a new Example.
   */
  def apply[L,T](label: L, features: T, id:String=""): Example[L,T] = {
    val l = label
    val f = features
    val i = id
    new Example[L,T] {
      val id = i
      val label = l
      val features = f
    }
  }

  /**
   * Lifts a function to operate over Examples,
   * Rather than the contained object.
   */
  def lift[T,U,L](f: T=>U) = (o : Example[L,T]) => o.map(f)

}


/**
 * Indexes the labels and features of a series of examples. Can be made much
 * more general, but just doing what is needed for the time being.
 */
class ExampleIndexer 
extends (Example[String,Seq[FeatureObservation[String]]]
         => Example[Int,Seq[FeatureObservation[Int]]]) {

  import nak.NakContext._
  import nak.util.GrowableIndex

  private[this] val lmap = new GrowableIndex[String]()
  private[this] val fmap = new GrowableIndex[String]()
  fmap("DUMMY FEATURE BECAUSE LIBLINEAR STARTS WITH 1-BASED INDEX")

  def apply(ex: Example[String,Seq[FeatureObservation[String]]]) =
    ex.relabel(lmap)
      .map(_.map(feature => feature.map(fmap)))
      .map(condense)

  def getMaps = (lmap.toMap, fmap.toMap)

}

/**
 * Indexes the labels and features of a series of examples. Can be made much
 * more general, but just doing what is needed for the time being.
 */
class HashedExampleIndexer(requestedHighestFeatureIndex: Int) 
extends (Example[String,Seq[FeatureObservation[String]]]
         => Example[Int,Seq[FeatureObservation[Int]]]) {

  import nak.NakContext._
  import nak.util.GrowableIndex

  private[this] val lmap = new GrowableIndex[String]()
  private[this] val fmap = HashedFeatureMap(requestedHighestFeatureIndex)

  val highestFeatureIndex = fmap.maxNumberOfFeatures

  def apply(ex: Example[String,Seq[FeatureObservation[String]]]) =
    ex.relabel(lmap)
      .map(_.map(feature => feature.map(f=>fmap.indexOfFeature(f).get)))
      .map(condense)

  def getMaps = (lmap.toMap, fmap)

}

