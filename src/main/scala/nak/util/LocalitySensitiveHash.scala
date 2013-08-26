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
  
  val randomHashFunctions =
    HashFunction.randomLinearHashFunctions(minHashLength)

  val documentShingles: Map[Int, Set[String]] =
    processedDocuments.map { document =>
      val shingles = document._1.toList.sliding(shingleLength)
        .map(_.mkString).toSet
      (document._2, shingles)
    }.toMap

  val shingleVocab =
    documentShingles.values.flatten.toSet.toIndexedSeq.zipWithIndex.toMap

  var mBands: IndexedSeq[Band] = null

  def findCandidates(shingles: Set[String]) = {
    val minHash = getMinHash(shingles)

    val subArrays = partitionArray(minHash).zipWithIndex

    val candidates = subArrays.par.flatMap { subArray =>
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
    val shingles = document.toLowerCase.sliding(shingleLength).toSet
    findCandidates(shingles).par.filter { candidate =>
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

    for {
      shingle <- shingles
      if shingleVocab.contains(shingle)
      shingleIndex = shingleVocab(shingle)
    } {

      var hashIndex = 0
      while (hashIndex < minHashLength) {
        val hf = randomHashFunctions(hashIndex)
        val permutedIndex = hf(shingleIndex) % shingleVocab.size
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
    (0 until numberBands).map { bandIndex =>
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

    val elementsPerBand = (1.0 * minHashLength / numberBands).ceil.toInt
    
    val bands = (0 until numberBands).par.map { bandIndex =>
      val start = bandIndex * elementsPerBand
      val end = start + elementsPerBand
      val subArray = minHashCollection.map { document =>
        (document._1, document._2.slice(start, end))
      }
      val band = new Band()
      subArray.foreach(band.hash)
      band
    }

    mBands = bands.toIndexedSeq
  }

}

object LocalitySensitiveHash { }

class HashFunction(slope: Int, const: Int) {
  def apply(x: Double) = slope*x + const
}

object HashFunction {

  def randomLinearHashFunctions(n: Int) = (0 until n).map { _=>
    val slope = util.Random.nextInt(1000)
    val const = util.Random.nextInt(1000)
    new HashFunction(slope, const)
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
  def apply(set1: Set[String], set2: Set[String]): Double =
    (set1 & set2).size.toDouble/(set1 | set2).size
}

object Example {
  def main(args:Array[String]) {
    val lines = io.Source.fromFile(args(0)).getLines
      .map(line => line.replaceAll("\\s+"," "))
      .map(line => line.toLowerCase)
      .toIndexedSeq
      .zipWithIndex

    // Hash all all documents read from file
    val lsh = new LocalitySensitiveHash(
      shingleLength=3,
      minHashLength=100,
      numberBands=10,
      lines,
      threshold = 0.5
    )

    // find the documents that are most similar to the below string
    val testString = "RT @Adam_Schefter: QB Tebow broke the combined record for QBs with a 38-inch vertical jump. He also ran an impressive 40 time"

    lsh.createHash()

    val similarEntries = lsh.findSimilar(testString.replaceAll("\\s+"," ").toLowerCase)
    for (index <- similarEntries)
      println(lines(index))

  }

}
