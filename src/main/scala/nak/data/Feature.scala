/*
 Copyright 2013 ScalaNLP
 
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
package nak.data

/**
 * A feature with its observed magnitude in some context. The default is
 * 1.0, which encodes the usual binary presence/absence distinction for
 * features.
 */
case class FeatureObservation[F](feature: F, magnitude: Double = 1.0) {

  def map[F2](f: F=>F2) = FeatureObservation(f(feature), magnitude)

  def mapOption[F2](f: F=>Option[F2]) = f(feature) match {
    case Some(result) => Some(FeatureObservation(result, magnitude))
    case None => None
  }

  def +(other: FeatureObservation[F]) = {
    assert(feature == other.feature)
    FeatureObservation(feature, magnitude + other.magnitude)
  }

  lazy val tuple = (feature, magnitude)

}


/**
 * A function that converts objects of some input class into a sequence
 * of FeatureObservations for an output class O.
 *
 * For text classification, I and O will typically be String. E.g. we
 * convert an entire document into the counts of all the words that
 * occur in it (see BowFeaturizer).
 */ 
trait Featurizer[I,O] extends (I => Seq[FeatureObservation[O]]) with Serializable

/**
 * A bag-of-words featurizer that simply tokenizes the input String by using
 * whitespace and creates an observation for each token.
 */ 
class BowFeaturizer(stopwords: Set[String] = Set[String]()) extends Featurizer[String, String] {
  def apply(raw: String) = raw
    .replaceAll("""([\?!\";\|\[\].,'])""", " $1 ")
    .trim
    .split("\\s+")
    .filterNot(stopwords)
    .map(tok => FeatureObservation("word="+tok))
}

/**
  * A function that converts a batch of objects of some input class into a
  * sequence of FeatureObservations for an output class O. Done for Examples,
  * so also must include the label type L.
  */
trait BatchFeaturizer[L,I,O]
    extends (Seq[Example[L,I]] => Seq[Example[L,Seq[FeatureObservation[O]]]])
    with Serializable

/**
  * A BatchFeaturizer that computes the tf-idf score of the terms in each
  * Example. Also performs basic feature selection by pruning words/features
  * that don't pass a given count.
  */
class TfidfBatchFeaturizer(minimumUnigramCount: Int = 2) 
    extends BatchFeaturizer[String,String,String] {

  import nak.util.CollectionUtil._
  import nak.util.CleanStringTokenizer

  def apply(examples: Seq[Example[String,String]]) = {
    val numDocuments = examples.length
    val documents = examples.map { ex =>
      ex.map(features=>CleanStringTokenizer(features.toLowerCase).counts)
    }
    val unigramFrequencies = collection.mutable.HashMap[String,Int]().withDefaultValue(0)
    val documentFrequencies = collection.mutable.HashMap[String,Int]().withDefaultValue(0)

    for (doc <- documents; (word,count) <- doc.features) {
      unigramFrequencies(word) += count
      documentFrequencies(word) += 1
    }

    val selectedWords = documentFrequencies.filter { wf =>
      wf._2 > 1 && unigramFrequencies(wf._1) >= minimumUnigramCount
    }

    val idfs = selectedWords.mapValues { docFreq =>
      math.log(.001+numDocuments/docFreq.toDouble)
    }

    for (doc <- documents) yield {
      doc.map { features =>
        (for ((word,termFrequency) <- features;  idf <- idfs.get(word)) yield
          FeatureObservation(word, termFrequency/idf)
        ).toSeq
      }
    }
  }
}


/**
 * A trait for classes that can index features represented as Strings. Non-general
 * at the moment.
 */ 
trait FeatureMap extends Serializable {
  def indexOfFeature(feature: String): Option[Int]
}

/**
 * A feature map that stores all feature strings and their indices in an in-memory Map.
 */ 
class ExactFeatureMap(val fmap: Map[String,Int]) extends FeatureMap {
  def indexOfFeature(feature: String) = fmap.get(feature)
}


/**
 * A feature map that uses the MurmurHash3 hash and mods on a prime giving the largest
 * feature index that can be used. Saves memory over an ExactFeatureMap because no explicit
 * map of Strings to Ints is maintained, and because you can use a model with fewer actual
 * parameters than features, if you can accept collisions. If the number of features used is
 * too small, you'll get a degradation in performance.
 *
 * For more details on the hashing trick, see:
 *   http://hunch.net/~jl/projects/hash_reps/index.html
 */ 
class HashedFeatureMap private(val maxNumberOfFeatures: Int) extends FeatureMap {
  import scala.util.hashing.MurmurHash3.stringHash
  private[this] def fmap: (String => Int) = featureString => 
    1 + (math.abs(stringHash(featureString)) % maxNumberOfFeatures)

  def indexOfFeature(feature: String) = Some(fmap(feature))

}

object HashedFeatureMap {

  /**
   * Construct a HashedFeatureMap by finding the greatest prime below the feature
   * bound. Obviously could be more efficient, but we pay the price once up front,
   * and it is reasonably fast up to 10,000,000 or so.
   */ 
  def apply(maxNumberOfFeatures: Int) = {
    val biggestPrimeBelow = primes.takeWhile(maxNumberOfFeatures>).last
    new HashedFeatureMap(biggestPrimeBelow)
  }

  /**
   * Took the simple code for computing primes from:
   *   http://stackoverflow.com/questions/6802112/why-is-this-scala-prime-generation-so-slow-memory-intensive
   */
  private lazy val primes = 2 #:: sieve(3)
  
  private def sieve(n: Int) : Stream[Int] =
    if (primes.takeWhile(p => p*p <= n).exists(n % _ == 0)) sieve(n + 2)
    else n #:: sieve(n + 2)

}
