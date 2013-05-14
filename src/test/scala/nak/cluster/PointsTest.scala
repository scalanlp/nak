package nak.cluster

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import nak.cluster._
import Jama.Matrix

class PointsTest extends FunSpec
  with ShouldMatchers {

  describe("PcaTransformer") {

    it("Compute the ZscoreTransformer and then a PcaTransformer and its cutoff.") {
      val pcaTransformer = PcaTransformer(source1)
      val transformedPoints = pcaTransformer(source1)
      assertPointsEqual(transformedPoints, transformed1)
    }
    
    it("inverse transorm") {
      val pcaTransformer = PcaTransformer(source1)
      val transformedPoints = pcaTransformer(source1)
      val retransformedPoints = pcaTransformer.inverseTransform(transformedPoints)
      assertPointsEqual(retransformedPoints, retransformed1)
    }
  }
  
  def assertPointsEqual(result: IndexedSeq[Point], expected: IndexedSeq[Point], tolerance: Double = 0.0001) {
    result.zip(expected).foreach {
        case (resultPoint, expectedPoint) =>
          resultPoint.coord.zip(expectedPoint.coord).foreach {
            case (resultCoord, expectedCoord) =>
              resultCoord should be(expectedCoord plusOrMinus tolerance)
          }
      }
  }

  object Points {
    def apply(coords: Int, data: Array[Double]): IndexedSeq[Point] = {
      data.sliding(coords, coords).map {
        new Point(_)
      }.toIndexedSeq
    }
  }

  val source1 = Points(2, Array(
    2.5, 2.4,
    0.5, 0.7,
    2.2, 2.9,
    1.9, 2.2,
    3.1, 3.0,
    2.3, 2.7,
    2.0, 1.6,
    1.0, 1.1,
    1.5, 1.6,
    1.1, 0.9));

  val transformed1 = Points(1, Array(
    0.34356009654506464,
    -0.7301500521577223,
    0.3927292539477773,
    0.10776488068856037,
    0.6907331559555456,
    0.3670580478507084,
    -0.029284168962485,
    -0.46868363020414905,
    -0.17937274736208175,
    -0.494354836301218));

  val retransformed1 = Points(2, Array(
    2.38226, 2.52693,
    0.59380, 0.59888,
    2.46416, 2.61522,
    1.98950, 2.10351,
    2.96054, 3.15034,
    2.42140, 2.56912,
    1.76122, 1.85741,
    1.02932, 1.06839,
    1.51122, 1.58790,
    0.98656, 1.02229));

}