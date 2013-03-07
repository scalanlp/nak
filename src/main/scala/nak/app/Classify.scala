package nak.app

import nak.classify._

/**
 * This is an example app for creating a Liblinear classifier from data that is 
 * stored as string valued features and string valued labels, e.g.
 * 
 * verb=join,noun=board,prep=as,prep_obj=director,V
 * verb=isIs,noun=chairman,prep=of,prep_obj=N.V.,N
 * verb=named,noun=director,prep=of,prep_obj=conglomerate,N
 *
 * These are examples from Ratnarparkhi's classic prepositional phrase attachment
 * dataset, discussed in the following homework:
 *
 *   http://ata-s12.utcompling.com/assignments/classification
 *
 * The homework includes pointers to the data and to Scala code for generating
 * said features.
 *
 * This example handles creating a feature index and getting the examples into the
 * right data structures for training with the logistic regression classifier,
 * which should serve as a useful example for creating features and classifiers
 * using the API.
 * 
 * @author jasonbaldridge
 */ 
object LiblinearClassifierFromCsv {

  import java.io._
  import de.bwaldvogel.liblinear._
  import nak.util.GrowableIndex

  def main(args: Array[String]) {

    val Array(trainfile,evalfile) = args

    // Feature map
    val lmap = new GrowableIndex[String]()
    val fmap = new GrowableIndex[String]()
    fmap("DUMMY FEATURE BECAUSE LIBLINEAR STARTS WITH 1-BASED INDEX")

    // Read in the training data and index it.
    val trainingData = 
      SparseCsvDataset(io.Source.fromFile(trainfile))
        .map { case(label, features) => {
          val indexedFeatures = features.map(fmap(_))
          val indexedLabel = lmap(label)
          (indexedLabel.toDouble, indexedFeatures)
        }}
        .toList // Need to consume the lines in order to populate the feature map

    val interceptFeature = (fmap("intercept"),1.0)

    val lmapFixed = lmap.toMap
    val fmapFixed = fmap.toMap

    val (responses, designMatrix) =
        trainingData
          .map{ case(label, features) => (label, makeLibsvmFeatureVector(features) ++ List(interceptFeature)) }
          .unzip

    // Train the classifier
    val classifier = 
      new LiblinearTrainer(1.0).applyIndexed(responses, designMatrix, fmapFixed.size)

    // Read in the evaluation data
    val evalData = 
      SparseCsvDataset(io.Source.fromFile(evalfile))
        .map { case(label, features) => {          
          val indexedFeatures = features.flatMap(fmapFixed.get)
          (lmapFixed(label), makeLibsvmFeatureVector(indexedFeatures) ++ List(interceptFeature))
        }}

    // Get the predictions
    val compare = evalData.map { case (label, features) =>
      (label, classifier(features.map{ case(a,v) => new FeatureNode(a,v) }).toInt)
    }.toList

    val correct = compare.count{ case(t,p) => t == p }
    println("Accuracy: " + correct/compare.length.toDouble*100)

  }

  /**
   * Creates a seq of libsvm format features from the example. Assumes we have
   * observations of each feature, and these could show up multiple times, so
   * we count those occurences and use those as the values stored in the
   * vector.
   */
  private def makeLibsvmFeatureVector (example: Seq[Int]) = {
    import nak.util.CollectionUtil._
    example.counts.mapValues(_.toDouble).toList.sorted
  }
  
  /**
   * Read in a dataset and create Examples from it. Don't do any feature indexation,
   * since for training data we want to build the index, but for eval data we just
   * want to use it.
   */
  object SparseCsvDataset {
  
    def apply(dataSource: io.Source): Iterator[(String, Seq[String])] =
      dataSource
        .getLines
        .zipWithIndex
        .map { case(line,rowId) => { 
          val lineData = line.split(",")
          val (features, label) = (lineData.dropRight(1), lineData.last)
          (label, features)
        }}

  }

}
