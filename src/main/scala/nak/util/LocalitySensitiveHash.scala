package nak.util

/**
  * A Locality Sensitive Hash that hashes the documents into buckets.
  * Only Jaccard Similarity is currently supported.
  *
  * @constructor : create a new instance
  * @param : shingleLength. Default value  = 3, 
  * @param : minHashLengh. The default value is = 100
  * @param : numBands. The Default value is 10.
  * @param : processedDocuments - which is a Tuple of (document , document index) The processed document can normalize the document by having just 
  * one space between words and converting all characters to lower case.
  * @param : threshold The default value for threshold is 0.8. 
  * 
  * The parameters numBands, threshold may be may be set so that
  * threshold is approximately equal to  (1/numBands)^(1/rows per band).
 **/
class LocalitySensitiveHash(
  documents: Iterable[String],
  shingleLength: Int = 5,
  numRows: Int = 100,
  numBands: Int=20) {

  import Similarity.jaccard
  import scala.util.hashing.MurmurHash3.stringHash
  
  private[this] val rowsPerBand =
    (numRows.toDouble / numBands).ceil.toInt

  val threshold = math.pow(1.0/numBands,1.0/rowsPerBand)
  
  private[this] val processedDocuments: IndexedSeq[String] =
    documents.par.map(StringCleaner.onlyAlpha).toIndexedSeq

  private[this] val randomHashFunctions: Seq[LinearHashFunction] =
    HashFunction.randomLinearHashFunctions(numRows)

  private[this] val documentShingles: IndexedSeq[Set[String]] =
    processedDocuments.par
      .map(text => text.sliding(shingleLength).toSet)
      .toIndexedSeq

  private[this] val shingleVocab = documentShingles.flatten.toSet

  private[this] def getShingleIndex: (String => Int) = { shingle =>
    math.abs(stringHash(shingle)) % Int.MaxValue
  }

  /**
    * Create the bands from the shingles.
    **/
  private[this] val mBands: IndexedSeq[Band] = {
    val minHashSignatures = documentShingles.par.map(getSignature).seq
    
    val bands = for {
      bandData <- minHashSignatures.transpose.grouped(rowsPerBand)
      subArraysForBand = bandData.toList.transpose.zipWithIndex
    } yield Band(subArraysForBand)

    bands.toIndexedSeq
  }

  /**
    * Returns documents that have Jaccard Similarity greater than threshold.
    * Assumes that documents have already been hashed.
    * 
    * @param : document . The document for which similar documents have to be identified
    **/
  def findSimilar(document: String) = {
    val shingles =
      StringCleaner.onlyAlpha(document).sliding(shingleLength).toSet
    
    val similarItems = getCandidates(shingles).par.filter { candidate =>
      jaccard(shingles, documentShingles(candidate)) > threshold
    }
    similarItems.seq.toSet
  }


  /**
    * Return the indices of candidates that collide in some band
    * with the given set of shingles.
    */
  def getCandidates(shingles: Set[String]) = {
    val bandsForCandidate =
      getSignature(shingles).grouped(rowsPerBand).toList

    val candidateLists = for {
      (subArray, index) <- bandsForCandidate.zipWithIndex.par
      bucket <- mBands(index).get(subArray)
    } yield bucket

    candidateLists.flatten.toSet
  }

  /**
    * Get the hash signature for a shingle set.
    */
  private[this] def getSignature(shingles: Set[String]) = {
    val minHash = Array.fill[Double](numRows)(Double.PositiveInfinity)
    shingles.filter(shingleVocab).map(getShingleIndex).foreach {
      shingleIndex =>
      // Using a while loop to be speedier (unfortunately).
      var hashIndex = 0
      while (hashIndex < numRows) {
        val hf = randomHashFunctions(hashIndex)
        val permutedIndex = hf(shingleIndex) % Int.MaxValue
        if (minHash(hashIndex) > permutedIndex)
          minHash(hashIndex) = permutedIndex
        hashIndex += 1
      }
    }
    minHash.toList
  }
}

/**
  * Simple line function: y = mx+b
  */ 
class LinearHashFunction(slope: Int, const: Int) {
  def apply(x: Double) = slope*x + const
}


/**
  * Helper object for hash function functions.
  */ 
object HashFunction {

  /** Get a sequence of random hash functions of the form mx+b. **/
  def randomLinearHashFunctions(n: Int) = {
    val functions = (0 until n).par.map { _=>
      val slope = util.Random.nextInt(1000)
      val const = util.Random.nextInt(1000)
      new LinearHashFunction(slope, const)
    }
    functions.seq
  }
}


/**
  * One band of the Locality Sensitive Hash.
  **/
class Band(buckets: Map[List[Double], List[Int]]) {

  /** Return the documents that collide to the same bucket. **/
  def get(subArray: List[Double]): Option[List[Int]] =
    buckets.get(subArray)

}

object Band {
  import CollectionUtil._
  
  /** Hash the sub-arrays into buckets to form a band. **/
  def apply(subArraysForBand: Seq[(List[Double], Int)]) =
    new Band(subArraysForBand.groupByKey.mapValues(_.toList))
}


object Similarity {
  /** Compute the Jaccard Similarity of two sets**/
  def jaccard(set1: Set[String], set2: Set[String]): Double =
    (set1 & set2).size.toDouble/(set1 | set2).size
}

object Example {
  def main(args:Array[String]) {
    val lines = io.Source.fromFile(args(0)).getLines.toIndexedSeq

    // Hash all all documents read from file
    val lsh = new LocalitySensitiveHash(lines, shingleLength=4)

    // find the documents that are most similar to the below string
    val testString = "RT @Adam_Schefter: QB Tebow broke the combined record for QBs with a 38-inch vertical jump. He also ran an impressive 40 time"

    val similarEntries = lsh.findSimilar(testString)
    for (index <- similarEntries)
      println(lines(index))
  }

}
