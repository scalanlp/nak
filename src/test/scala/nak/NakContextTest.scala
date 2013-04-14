package nak

import org.scalatest.FunSpec

import nak.data.FeatureObservation

/**
 * Make sure NakContext functions work as expected.
 */ 
class NakContextSpec extends FunSpec {

  import NakContext._

  describe("feature observation condensing") {

    it ("should merge counts and sort") {
      val orig = 
        Seq(FeatureObservation(1,1.0),
            FeatureObservation(0,1.0),
            FeatureObservation(1,2.0))

      val goal = 
        Seq(FeatureObservation(0,1.0),
            FeatureObservation(1,3.0))

      assert(goal === condense(orig))
    }
  }



}
