package nak.data

/*
 Copyright 2013 Jason Baldridge
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

/**
 * A feature with its observed magnitude in some context. The default is
 * 1.0, which encodes the usual binary presence/absence distinction for
 * features.
 *
 * @author jasonbaldridge
 */
case class FeatureObservation[F](feature: F, magnitude: Double = 1.0) {

  def map[F2](f: F=>F2) = FeatureObservation(f(feature), magnitude)

  def mapOption[F2](f: F=>Option[F2]) = f(feature) match {
    case Some(result) => Some(FeatureObservation(result, magnitude))
    case None => None
  }

  def +(other: FeatureObservation[F]) = {
    assert(feature == other.feature)
    FeatureObservation(feature, magnitude + other.magnitude)
  }

  lazy val tuple = (feature, magnitude)

}
