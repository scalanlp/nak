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

  private[this] val rowsPerBand =
    (numRows.toDouble / numBands).ceil.toInt

  val threshold = math.pow(1.0/numBands,1.0/rowsPerBand)
  
  private[this] val processedDocuments: IndexedSeq[String] =
    documents.map(StringCleaner.onlyAlpha).toIndexedSeq

  private[this] val randomHashFunctions: Seq[HashFunction] =
    HashFunction.randomLinearHashFunctions(numRows)

  private[this] val documentShingles: IndexedSeq[Set[String]] =
    processedDocuments.par
      .map(text => text.sliding(shingleLength).toSet)
      .toIndexedSeq

  private[this] val shingleVocab = documentShingles.flatten.toSet

  import scala.util.hashing.MurmurHash3.stringHash
  private[this] def getShingleIndex: (String => Int) = { shingle =>
    math.abs(stringHash(shingle)) % Int.MaxValue
  }

  /**
    * Create the bands from the shingles.
    **/
  private[this] val mBands: IndexedSeq[Band] = {
    val minHashCollection = documentShingles.map(getMinHash)
    val rowsPerBand = (numRows.toDouble / numBands).ceil.toInt
    val bands = (0 until numBands).par.map { bandIndex =>
      val start = bandIndex * rowsPerBand
      val end = start + rowsPerBand
      val subArray = minHashCollection.zipWithIndex.map {
        case (docHash, docIndex) => (docIndex, docHash.slice(start, end))
      }
      val band = new Band()
      subArray.foreach(band.hash)
      band
    }
    bands.toIndexedSeq
  }
  
  def findCandidates(shingles: Set[String]) = {
    val minHash = getMinHash(shingles)
    val bandsForCandidate = minHash.grouped(rowsPerBand).toIndexedSeq
    val subArrays = bandsForCandidate.zipWithIndex
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
    val shingles =
      StringCleaner.onlyAlpha(document).sliding(shingleLength).toSet
    
    val similarItems = findCandidates(shingles).par.filter { candidate =>
      val js = JaccardSimilarity(shingles, documentShingles(candidate.toInt))
      js > threshold
    }
    similarItems.seq.toSet
  }

  def getMinHash(shingles: Set[String]) = {

    val minHash = Array.fill[Double](numRows)(Double.PositiveInfinity)
    
    for {
      shingle <- shingles
      if shingleVocab(shingle)
      shingleIndex = getShingleIndex(shingle)
    } {
      // Use a while loop to be speedier (unfortunately).
      var hashIndex = 0
      while (hashIndex < numRows) {
        val hf = randomHashFunctions(hashIndex)
        val permutedIndex = hf(shingleIndex) % Int.MaxValue
        if (minHash(hashIndex) > permutedIndex)
          minHash(hashIndex) = permutedIndex
        hashIndex += 1
      }
    }
    minHash
  }
}

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
    val subList = subArray._2.toList
    buckets.get(subList) match {
      case Some(value) => value += subArray._1
      case None => buckets(subList) = ArrayBuffer(subArray._1)
    }
  }

  /** Return the documents that collide to the same bucket **/
  def getCollisionObjects(subArray: Array[Double]): Option[List[Int]] = {
    val subList = subArray.toList
    buckets.get(subList) match {
      case Some(value) =>
        Some(value.toList)

      case None =>
        buckets(subList) = ArrayBuffer(-1)
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
