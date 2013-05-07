package nak.util

import nak.data.Example
import nak.core.FeaturizedClassifier
import nak.core.IndexedClassifier

case class Scores(accuracy: Double, precisionAverage: Double, recallAverage: Double, fscoreAverage: Double, all: Seq[Seq[Double]] )

/**
 * A confusion matrix for comparing gold clusters to some predicted clusters.
 *
 * @param goldLabels	the set of labels
 * @param counts the matrix, where each cell is the number of data points that had a given gold label and predicted label
 */
class ConfusionMatrix[L, I](
  labels: Seq[L],
  counts: Seq[Seq[Int]],
  examples: Seq[Seq[Seq[I]]]) {

  lazy val numLabels = labels.length

  lazy val lineSeparator = "-" * 80 + "\n"

  def safeDivide(num: Double, denom: Double) =
    if (denom == 0.0) 0.0 else num / denom

  def formatPercent(percent: Double) = "%1.2f".format(100 * percent)

  lazy val scores = {
    // Calculate accuracy, precision, recall, and F-score
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
    Scores(accuracy, precisionAverage, recallAverage, fscoreAverage, prfs)
  }

  lazy val measurements = {
    (lineSeparator
      + "\t" * 2 + formatPercent(scores.accuracy) + "\tOverall accuracy\n"
      + lineSeparator + "P\tR\tF\n"
      + scores.all.zip(labels).map {
        case (prf, label) => prf.map(formatPercent).mkString("\t") + "\t" + label
      }.mkString("\n")
      + "\n" + "." * 35 + "\n"
      + formatPercent(scores.precisionAverage) + "\t"
      + formatPercent(scores.recallAverage) + "\t"
      + formatPercent(scores.fscoreAverage) + "\tAverage")
  }

  lazy val detailedOutput = {
    val sep = "-" * 80 + "\n"
    val correct =
      for (i <- 0 until numLabels) yield "Correctly labeled: " + labels(i) + "\n" + sep + examples(i)(i).mkString("\n\n")

    val incorrect =
      for (
        i <- 0 until numLabels;
        j <- 0 until numLabels;
        if i != j
      ) yield {
        ("Incorrectly labeled: " + labels(i) + " mistaken as " + labels(j) + "\n" + sep
          + examples(i)(j).mkString("\n\n"))
      }
    (sep + "CORRECTLY LABELED EXAMPLES\n" + sep + correct.mkString("\n\n\n")
      + "\n\n\n" + sep + "INCORRECTLY LABELED EXAMPLES\n" + sep + incorrect.mkString("\n\n\n"))
  }

  // Create a string representation. Be lazy so that we only do it once.
  lazy val stringRep = {
    val lengthOfRow = if (counts.length > 0) counts(0).mkString.length + counts(0).length * 7
    else 0

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
      labels.mkString("\t") + "\n"
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

  def apply[L,I](goldLabels: Seq[L], predictedLabels: Seq[L], items: Seq[I])(implicit ord: Ordering[L]) = {

    val labels = (goldLabels.toSet ++ predictedLabels.toSet).toIndexedSeq.sorted
    val labelIndices = labels.zipWithIndex.toMap
    val numLabels = labels.length
    val counts = Array.fill(numLabels, numLabels)(0)
    val examples = Array.fill(numLabels, numLabels)(new ListBuffer[I])

    goldLabels.zip(predictedLabels).zip(items).foreach {
      case ((goldLabel, predLabel), item) =>
        counts(labelIndices(goldLabel))(labelIndices(predLabel)) += 1
        examples(labelIndices(goldLabel))(labelIndices(predLabel)) += item
    }

    new ConfusionMatrix(
      labels,
      counts.map(_.toIndexedSeq).toIndexedSeq,
      examples.map(_.toIndexedSeq).toIndexedSeq)

  }

}

object CrossValidation {

  /**
   * Runs a N-Fold cross-validation and returns a ConfusionMatrix with all the results (i.e. you get an average result over all folds). XS, the set of examples is split in `nbrFold` subsets. We then run `nbrFold` evaluation, where we test on the i-th subset and train on all the other one. For instance if we have `xs = Seq(1,2,3,4)` with `nbrFold=4`, we will run 4 evaluations:
   * - test= Seq(1), train = Seq(2,3,4)
   * - test= Seq(2), train = Seq(1,3,4)
   * - test= Seq(3), train = Seq(1,2,4)
   * - test= Seq(4), train = Seq(1,2,3)
   */
  def crossValidation[L,I](xs: Traversable[Example[L, I]], nbrFold: Int)(f: Traversable[Example[L, I]] => IndexedClassifier[L] with FeaturizedClassifier[L, I])(implicit ord: Ordering[L]): ConfusionMatrix[L,I] = {
    val size = (xs.size / nbrFold).ceil.toInt
    val tests = for {
      fold <- 0 until nbrFold
    } yield {
      val test = xs.slice(fold * size, (fold + 1) * size)
      val train = xs.slice(0, fold * size) ++ xs.slice((fold + 1) * size, xs.size)

      val classifier = f(train)

      (for {
        t <- test
      } yield (t.label, classifier.predict(t.features), t.features))
    }
    val testZ = tests.flatten.unzip3
    ConfusionMatrix(testZ._1, testZ._2, testZ._3)
  }

  /**
   * Runs a leave one out evaluation. This is equivalent to a n-fold cross-validation where the number of fold is equal to the number of examples. That is, we take one example out, we train on all other examples and test on the example that we reserved, this is repeated for each example.
   * While this is useful for cases where there aren't many examples, it might be quite slow for large datasets and a n-fold with a smaller number of splits might yield a good evaluation anyway.
   */
  def leaveOneOut[L,I](xs: Traversable[Example[L, I]])(f: Traversable[Example[L, I]] => IndexedClassifier[L] with FeaturizedClassifier[L, I])(implicit ord: Ordering[L]): ConfusionMatrix[L,I] = {
    crossValidation(xs, xs.size)(f)
  }

}
