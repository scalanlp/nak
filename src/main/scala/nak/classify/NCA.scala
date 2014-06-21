/*
 * dialogue -- The University of Washington Dialogue Framework
 *
 * Copyright 2013 - Gabriel Schubiner
 *
 * NCA.scala is part of dialogue.
 *
 * dialogue is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * dialogue is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with dialogue.  If not, see <http://www.gnu.org/licenses/>.
 */

package nak.classify

import breeze.linalg.{sum, DenseMatrix, Counter}
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize.StochasticDiffFunction
import breeze.util.Isomorphism
import nak.data.Example

/**
 * dialogue
 * 6/19/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
class NCA[L,T](examples: Iterable[Example[L,T]], A: DenseMatrix[Double]) extends Classifier[L,T] {
  /** For the observation, return the score for each label that has a nonzero
    * score.
    */
  override def scores(o: T): Counter[L, Double] = ???
}

object NCA {

  class Trainer[L,T](opt: OptParams = OptParams()) extends Classifier.Trainer[L,T] {
    override type MyClassifier = NCA[L,T]

    override def train(data: Iterable[Example[L, T]]): MyClassifier = {

      implicit val exampleVectorIsomorphism = Isomorphism[Example[L,T],T]((t: Example[L,T]) => t.features, (u: T) => new Example[L,T] {
        override def id: String = ""

        override def label: L = ???

        override def features: T = ???
      })

      val f = new StochasticDiffFunction[DenseMatrix[Double]] {
        override def calculate(x: T): (Double, T) = {
          //Expected number of points correctly classified
          val value = sum(data.filter(_.label == x))
        }
      }
    }
  }
}
