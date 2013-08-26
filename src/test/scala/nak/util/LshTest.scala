package nak.util

import org.scalatest.FunSpec

/**
 * Test LocalitySensitiveHash using Twitter data.
 */ 
class LocalitySensitiveHashSpec extends FunSpec {

    describe("Locality Sensitive Hash") {
    val tweets = this.getClass.getResourceAsStream(
      "/data/twitter/example_tweets.txt")

    it ("should index and find near matches") {
      val lines = io.Source.fromInputStream(tweets).getLines
        .map(StringCleaner.onlyAlpha)
        .toIndexedSeq
      
      // Hash all all documents read from file
      val lsh = new LocalitySensitiveHash(
        lines, shingleLength=3, numRows=100, numBands=10)
      
      // find the documents that are most similar to the below string
      val testString = "RT @Adam_Schefter:QB Tebow broke the combined record for QBs with a 38-inch vertical jump. He also ran an impressive 40 time "
      
      val matchingIds = lsh.findSimilar(testString).toList.sorted
      println("** " + matchingIds)
      
      // Need to deal better with randomness
      assert((matchingIds == List(34,193,851)) | (matchingIds == List(851)))
    }
  }

}

