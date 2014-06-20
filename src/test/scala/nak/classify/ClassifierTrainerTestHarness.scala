package nak.classify

import nak.space.dm.DMImplicits.euclidean
import org.scalatest.FunSuite
import nak.data.{Datasets, DataMatrix, Example}
import nak.stats.ContingencyStats
import breeze.linalg._

/**
 * 
 * @author dlwh
 */

trait ClassifierTrainerTestHarness extends FunSuite {
  def trainer[L,F]: Classifier.Trainer[L,Counter[F,Double]]

  test("simple example") {
    val trainingData = Array (
      Example("cat",Counter.count("fuzzy","claws","small").mapValues(_.toDouble)),
      Example("bear",Counter.count("fuzzy","claws","big").mapValues(_.toDouble)),
      Example("cat",Counter.count("claws","medium").mapValues(_.toDouble))
    )
    val testData = Array(
      Example("cat", Counter.count("claws","small").mapValues(_.toDouble))
    )

    val r = trainer[String,String].train(trainingData).classify(testData(0).features)
    assert(r == testData(0).label)
  }

}

trait ContinuousTestHarness extends ClassifierTrainerTestHarness {
  test("prml") {
    val classifier = trainer[Int,Int].train(PRMLData.classification)
    val contingencyStats = ContingencyStats(classifier, PRMLData.classification)
    assert(contingencyStats.microaveraged.precision > 0.65,contingencyStats)
  }
}

trait NearestNeighborTestHarness extends FunSuite {
  def trainer[L]: Classifier.Trainer[L,DenseVector[Double]]

  test("iris-LOO") {
    type DS = IndexedSeq[Example[String,DenseVector[Double]]]
    val testLOO: (DS,DS) => Boolean = (train: DS, test: DS) => {
      val nnC = new kNearestNeighbor.Trainer[String,DenseVector[Double],euclidean.type](3).train(train)
      nnC.classify(test.head.features) == test.head.label
    }
    val looCV = Datasets.loocv[Example[String,DenseVector[Double]]](IrisData.classification.toIndexedSeq)
    val res = looCV[Boolean](testLOO)
    assert(res.count(identity).toDouble / res.size > 0.90)
  }
}

// Data from Bishop
object PRMLData {
  val classification = {
    val url = PRMLData.getClass().getClassLoader().getResource("data/classify/prml")
    val datamatrix = DataMatrix.fromURL[Int](url,3, labelReader = _.toDouble.toInt)
    datamatrix.rows.map { ex =>
      ex.map{row =>
        val r = Counter[Int,Double]()
        row.foreachKey(k => r(k) = row(k))
        r
      }
    }
  }
}

object IrisData {
  val classification = {
    val url = IrisData.getClass.getClassLoader.getResource("data/classify/iris.data")
    DataMatrix.fromURL[String](url,4,separator = ",").rows
  }
}
