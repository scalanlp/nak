/*
 Copyright 2013 Jason Baldridge

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
package nak.cluster

object ClusterReport {

  import nak.util.CollectionUtil._

  /**
   * Given the ids (descriptors) for each point, their true label and the id
   * for the cluster they have been assigned, print all the ids for each 
   * label-cluster_id.
   * 
   * @param ids the sequence of descriptors for each point
   * @param labels the sequence of true cluster labels for each point
   * @param predictedClusterIds the ids of the clusters predicted by k-means
   */
  def apply(ids: Seq[String], labels: Seq[String], predictedClusterIds: Seq[Int]) {
    labels.zip(predictedClusterIds).zip(ids).groupByKey.foreach {
      case ((label, cluster), idsInCluster) =>
        println("[Label: " + label + "] <=> [Cluster: " + cluster + "]")
        println("Ids: " + idsInCluster.mkString("\t"))
        println
    }
  }
}
