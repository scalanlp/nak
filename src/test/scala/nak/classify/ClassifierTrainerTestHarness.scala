package nak.classify

import nak.space.DMImplicits
import DMImplicits.euclidean
import org.scalatest.FunSuite
import nak.data.{Datasets, DataMatrix, Example}
import nak.stats.ContingencyStats
import breeze.linalg._

import scala.reflect.runtime.universe._
import scala.util.Random

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

trait DenseNearestNeighborTestHarness extends FunSuite {
  def trainer[L]: Classifier.Trainer[L,DenseVector[Double]]

  test("iris-CV") {
    type DS = IndexedSeq[Example[String,DenseVector[Double]]]
    var i = 1
    val k = 3
    val testCV: (DS,DS) => Double = (train: DS, test: DS) => {
      i += 1
      val nnC = trainer.train(train)
      test.foldLeft(0)({case (iSum,ex) =>
        if (nnC.classify(ex.features) == ex.label) iSum + 1 else iSum}).toDouble / test.size
    }
    val r = new Random(1)
    val crossV = Datasets.crossValidate[Example[String,DenseVector[Double]],Double](k,r.shuffle(IrisData.denseClassification.toIndexedSeq))(testCV)
    assert(!crossV.exists(_ < 0.90))
  }
}

trait SparseNearestNeighborTestHarness extends FunSuite {
  def trainer[L]: Classifier.Trainer[L,SparseVector[Double]]

  test("iris-CV") {
    type DS = IndexedSeq[Example[String,SparseVector[Double]]]
    var i = 1
    val k = 3
    val testCV: (DS,DS) => Double = (train: DS, test: DS) => {
      i += 1
      val nnC = trainer.train(train)
      test.foldLeft(0)({case (iSum,ex) =>
        if (nnC.classify(ex.features) == ex.label) iSum + 1 else iSum}).toDouble / test.size
    }
    val r = new Random(1)
    val crossV = Datasets.crossValidate[Example[String,SparseVector[Double]],Double](k,r.shuffle(IrisData.sparseClassification.toIndexedSeq))(testCV)
    assert(!crossV.exists(_ < 0.90))
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
  val url = IrisData.getClass.getClassLoader.getResource("data/classify/iris.data")
  val dm = DataMatrix.fromURL[String](url,4,separator = ",")

  def sparseClassification: Seq[Example[String,SparseVector[Double]]] =
    dm.rows.map(e => e.map(f => SparseVector(f.data)))

  def denseClassification: Seq[Example[String,DenseVector[Double]]] = dm.rows

  val size = dm.rows.size
}
