/*
 * dialogue -- The University of Washington Dialogue Framework
 *
 * Copyright 2013 - Gabriel Schubiner
 *
 * minkowski.scala is part of dialogue.
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

package nak.space.dm

import breeze.generic.UFunc
import breeze.linalg.{ZippedValues, zipValues}
import breeze.numerics._

/**
 * dialogue
 * 6/8/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
object minkowski extends UFunc {
  implicit def minkowskiDistanceFromZippedValues[T, U]
  (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl3[T, U, Double, Double] =
    new Impl3[T, U, Double, Double] {
      def apply(v: T, v2: U, v3: Double): Double = {
        var cumul = 0.0
        zipValues(v,v2).foreach {
          (a,b) =>
            cumul += pow(abs(a - b),v3)
        }
        pow(cumul,1/v3)
      }
    }
}
