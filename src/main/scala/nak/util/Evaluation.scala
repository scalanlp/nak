package nak.util

/**
 * A confusion matrix for comparing gold clusters to some predicted clusters.
 *
 * @param goldLabels	the set of labels
 * @param counts the matrix, where each cell is the number of data points that had a given gold label and predicted label
 */
class ConfusionMatrix(
  labels: Seq[String], 
  counts: Seq[Seq[Int]],
  examples: Seq[Seq[Seq[String]]]) {

  lazy val numLabels = labels.length

  lazy val lineSeparator = "-" * 80 + "\n"

  def safeDivide(num: Double, denom: Double) =
    if (denom == 0.0) 0.0 else num / denom

  def formatPercent(percent: Double) = "%1.2f".format(100 * percent)

  // Calculate accuracy, precision, recall, and F-score
  lazy val measurements = {
    val goodCounts = counts.indices.map(index => counts(index)(index))
    val totalCounts = counts.map(_.sum).sum.toDouble
    val accuracy = goodCounts.sum / totalCounts

    val recallDenominators = counts.map(_.sum.toDouble)
    val recallValues = counts.indices.map { index =>
      safeDivide(goodCounts(index), recallDenominators(index))
    }

    val precisionDenominators = counts.transpose.map(_.sum.toDouble)
    val precisionValues = counts.indices.map { index =>
      safeDivide(goodCounts(index), precisionDenominators(index))
    }

    val fscores = precisionValues.zip(recallValues).map {
      case (p, r) => safeDivide(2 * p * r, p + r)
    }

    val prfs = Seq(precisionValues, recallValues, fscores).transpose

    val precisionAverage = precisionValues.sum / numLabels
    val recallAverage = recallValues.sum / numLabels
    val fscoreAverage = fscores.sum / numLabels

    (lineSeparator
      + "\t" * 2 + formatPercent(accuracy) + "\tOverall accuracy\n"
      + lineSeparator + "P\tR\tF\n"
      + prfs.zip(labels).map {
        case (prf, label) => prf.map(formatPercent).mkString("\t") + "\t" + label
      }.mkString("\n")
      + "\n" + "."*35 + "\n"
      + formatPercent(precisionAverage) + "\t" 
      + formatPercent(recallAverage) + "\t" 
      + formatPercent(fscoreAverage) + "\tAverage")
  }

  lazy val detailedOutput = {
    val sep = "-"*80 + "\n"
    val correct = 
      for (i <- 0 until numLabels) yield
	"Correctly labeled: " + labels(i) + "\n" + sep + examples(i)(i).mkString("\n\n")

    val incorrect = 
      for (i <- 0 until numLabels;
	   j <- 0 until numLabels;
	   if i != j) 
      yield {
	("Incorrectly labeled: " + labels(i) + " mistaken as " + labels(j) +  "\n" + sep
	 + examples(i)(j).mkString("\n\n"))
      }
    (sep + "CORRECTLY LABELED EXAMPLES\n" + sep + correct.mkString("\n\n\n")
     + "\n\n\n" + sep + "INCORRECTLY LABELED EXAMPLES\n" + sep + incorrect.mkString("\n\n\n"))
  }

  // Create a string representation. Be lazy so that we only do it once.
  lazy val stringRep = {
    val lengthOfRow = counts(0).mkString.length + counts(0).length * 7

    val tableString =
      counts.zip(labels)
        .map {
          case (goldLine, goldLabel) =>
            (goldLine.mkString("\t") + "\t|\t" + goldLine.sum + "\t" + goldLabel)
        }
        .mkString("\n")

    ("-" * 80 + "\n" + "Confusion matrix.\n" +
      "Columns give predicted counts. Rows give gold counts.\n" +
      "-" * 80 + "\n" +
      tableString + "\n" +
      "-" * lengthOfRow + "\n" +
      counts.transpose.map(_.sum).mkString("\t") + "\n" +
      labels.mkString(" ") + "\n"
      + "\n" + measurements)
  }

  // Get the string representation.
  override def toString = stringRep
}

/**
 * A companion object for constructing ConfusionMatrices.
 */
object ConfusionMatrix {

  import scala.collection.mutable.ListBuffer

  def apply(goldLabels: Seq[String], predictedLabels: Seq[Option[String]], items: Seq[String]) = {

    val labels = (goldLabels.toSet ++ predictedLabels.flatten.toSet).toIndexedSeq.sorted
    val labelIndices = labels.zipWithIndex.toMap
    val numLabels = labels.length
    val counts = Array.fill(numLabels, numLabels)(0)
    val examples = Array.fill(numLabels, numLabels)(new ListBuffer[String])

    goldLabels.zip(predictedLabels).zip(items).foreach {
      case ((goldLabel, Some(predLabel)), item) =>
        counts(labelIndices(goldLabel))(labelIndices(predLabel)) += 1
        examples(labelIndices(goldLabel))(labelIndices(predLabel)) += item
      case _ => ;
    }

    new ConfusionMatrix(
      labels, 
      counts.map(_.toIndexedSeq).toIndexedSeq, 
      examples.map(_.toIndexedSeq).toIndexedSeq)

  }

}
