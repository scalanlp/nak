package nak.dimreduction

object Example {
  def main(args:Array[String]) {
    val lines = io.Source.fromFile(args(0)).getLines
      .map(line => line.split("\\s+").mkString(" "))
      .map(line => line.toLowerCase)
      .toIndexedSeq
      .zipWithIndex
    
    // Hash all all documents read from file
    val lsh = new LSH(
      shingleLength=3,
      minHashLength=100,
      numberBands=10,
      lines,
      threshold = 0.2
    )
    
    // find the documents that are most similar to the below string
    val testString = "RT @Adam_Schefter: QB Tebow broke the combined record for QBs with a 38-inch vertical jump. He also ran an impressive 40 time"
    
    lsh.createHash()

    val similarEntries = lsh.findSimilar(testString.replaceAll("\\s+"," "))
    for (index <- similarEntries)
      println(lines(index))
    
  }
  
}

