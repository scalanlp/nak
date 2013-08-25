package nak.util

/**
  * A Locality Sensitive Hash that hashes the documents into buckets.
  * Only Jaccard Similarity is currently supported.
  *
  * @constructor : create a new instance
  * @param : shingleLength. Default value  = 3, 
  * @param : minHashLengh. The default value is = 100
  * @param : numberBands. The Default value is 10.
  * @param : processedDocuments - which is a Tuple of (document , document index) The processed document can normalize the document by having just 
  * one space between words and converting all characters to lower case.
  * @param : threshold The default value for threshold is 0.8. 
  * 
  * The parameters numberBands, threshold may be may be set so that
  * threshold is approximately equal to  (1/numberBands)^(1/rows per band).
 **/
class LocalitySensitiveHash(
  shingleLength: Int = 3,
  minHashLength: Int = 100,
  numberBands: Int=10,
  processedDocuments: IndexedSeq[(String, Int)],
  threshold: Double=0.8) {

  val randomHashFunctions = randomLinearHashFunction(minHashLength)

  val documentShingles: Map[Int, Set[String]] =
    processedDocuments.map { document =>
      val shingles = document._1.toList.sliding(shingleLength)
        .map(_.mkString).toSet
      (document._2, shingles)
    }.toMap

  val shingleVocab =
    documentShingles.values.flatten.toSet.toIndexedSeq.zipWithIndex.toMap

  var mBands: IndexedSeq[Band] = null

  private def randomLinearHashFunction(n: Int) = {
    val slope = scala.util.Random.shuffle(0 to 1000)
    val const = scala.util.Random.shuffle(0 to 1000)
    slope.zip(const).take(minHashLength)
  }

  def findCandidates(shingles: Set[String]) = {
    val minHash = getMinHash(shingles)

    val subArrays = partitionArray(minHash).zipWithIndex

    val candidates = subArrays.flatMap { subArray =>
      val index = subArray._2
      val hashedBucket = mBands(index).getCollisionObjects(subArray._1)
      hashedBucket
    }.flatten.toSet

    candidates
  }

  /**
    * Returns documents that have Jaccard Similarity greater than threshold.
    * Assumes that documents have already been hashed.
    * 
    * @param : document . The document for which similar documents have to be identified
    **/
  def findSimilar(document: String) = {
    val shingles = document.toList.sliding(shingleLength)
      .map(_.mkString)
      .map(shingle => shingle.toLowerCase)
      .toSet

    findCandidates(shingles).filter { candidate =>
      val js = JaccardSimilarity(shingles, documentShingles(candidate.toInt))
      js > threshold
    }
  }

  /**
    * Returns the Min Hash of a document
    * 
    * @param : The shingle representation for that document
    **/
  def getMinHash(shingles: Set[String]) = {

    val minHash = Array.fill[Double](minHashLength)(Double.PositiveInfinity)

    shingles.filter(x => shingleVocab.contains(x)).foreach { shingle =>
      val shingleIndex = shingleVocab(shingle)
      var hashIndex = 0
      randomHashFunctions.foreach { function =>
        val permutedIndex =
          (function._1 * shingleIndex + function._2) % shingleVocab.size
        
        if (minHash(hashIndex) > permutedIndex)
          minHash(hashIndex) = permutedIndex
        
        hashIndex += 1
      }
    }
    minHash
  }

  /**
    * Partition the min-hash into numberBands bands
    * 
    * @param : The shingle representation of the document
    **/
  def partitionArray(minHash: Array[Double]): IndexedSeq[Array[Double]] = {

    if (minHash.length < numberBands) {
      println("number of bands exceeds minHash")
      System.exit(0)
    }

    val elementsPerBand = (minHash.length / numberBands)
    (0 to numberBands - 1).map { bandIndex =>
      val start = bandIndex * elementsPerBand
      val end = start + elementsPerBand
      minHash.slice(start, end)
    }
  }

  /**
    * Create a locality sensitive hash for the all the processed documents.
    **/
  def createHash() = {

    val minHashCollection =
      documentShingles.mapValues(shingleSet => getMinHash(shingleSet))

    val bands = (0 to numberBands - 1).map { bandIndex =>
      val elementsPerBand = (1.0 * minHashLength / numberBands).ceil.toInt
      val start = bandIndex * elementsPerBand
      val end = start + elementsPerBand
      val subArray = minHashCollection.map { document =>
        (document._1, document._2.slice(start, end))
      }
      val band = new Band()
      subArray.foreach(array => band.hash(array))
      band
    }

    mBands = bands
  }

}

/**
  * One band of the Locality Sensitive Hash.
  **/
class Band {
  import scala.collection.mutable.ArrayBuffer
  val buckets =
    scala.collection.mutable.Map[List[Double], ArrayBuffer[Int]]()

  /** Hash the sub-array into buckets **/
  def hash(subArray: (Int, Array[Double])) {
    buckets.get(subArray._2.toList) match {
      case Some(value: ArrayBuffer[Int]) => value += subArray._1
      case None => buckets(subArray._2.toList) = ArrayBuffer(subArray._1)
    }
  }

  /** Return the documents that collide to the same bucket **/
  def getCollisionObjects(subArray: Array[Double]): Option[List[Int]] = {
    buckets.get(subArray.toList) match {
      case Some(value: ArrayBuffer[Int]) =>
        Some(value.toList)

      case None =>
        buckets(subArray.toList) = ArrayBuffer(-1)
        None
    }
  }

}

/** Compute the Jaccard Similarity of two sets**/
object JaccardSimilarity {
  def apply(set1: Set[String], set2: Set[String]): Double = {
    val intersection = set1.intersect(set2).size
    val union = set2.union(set2).size
    (intersection * 1.0) / union
  }
}

