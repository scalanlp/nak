import nak.dimreduction.LSH
import scala.io.Source;


object Example {
    def main(args:Array[String]) {
        val lines = Source.fromFile(args(0)).getLines
        .map(line => line.split("\\s+").mkString(" "))
        .map(line => line.toLowerCase)
        .toIndexedSeq
        .zipWithIndex

        // Hash all all documents read from file
        val lsh = new LSH(shingleLength=3,
            minHashLength = 100,numberBands=10,lines,threshold = 0.7);

        // find the documents that are most similar to the below string
        val string = "Hi what is your     name ????"

        lsh.createHash();
        println(lsh.findSimilar(string.split("\\s+").mkString(" ")));
    }
}

