/*
 * dialogue -- The University of Washington Dialogue Framework
 *
 * Copyright 2013 - Gabriel Schubiner
 *
 * manhattan.scala is part of dialogue.
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

import breeze.linalg.{ZippedValues, zipValues}
import breeze.numerics._
import breeze.generic.UFunc

/**
 * dialogue
 * 6/8/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
object manhattan extends UFunc {
  implicit def manhattanDistanceFromZippedValues[T, U]
  (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl2[T, U, Double] =
    new Impl2[T, U, Double] {
      def apply(v: T, v2: U): Double = {
        var dist = 0.0
        zipValues(v, v2).foreach {
          (a, b) =>
            dist += abs(a - b)
        }
        dist
      }
    }
}
