package nak.app

import nak.core._

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
 * This example uses Nak's data indexers and event streams to read in the data, and
 * and it also tests model writing and reading.
 * 
 * @author jasonbaldridge
 */ 
object LiblinearExample {

  import java.io._
  import nak.liblinear._
  import nak.util.GrowableIndex
  import nak.data._
  import nak.io._
  import nak.core._

  def main(args: Array[String]) {

    val Array(trainfile,evalfile) = args

    val trainingReader = new BufferedReader(new FileReader(trainfile))
    val dataIndexer = new TwoPassDataIndexer(new BasicEventStream(new PlainTextByLineDataStream(trainingReader), ","))

    // Train the model
    val classifierTrained = LiblinearTrainer.train(dataIndexer)

    // Write the model to disk
    val modelFile = "/tmp/example-model.bin.gz"
    new GenericModelWriter(classifierTrained, new File(modelFile)).persist

    // Read the model from disk
    val classifierReader = new GenericModelReader(new File(modelFile))
    classifierReader.checkModelType
    val classifier = classifierReader.constructModel

    // Read in the evaluation data
    val evalReader = new BufferedReader(new FileReader(evalfile))
    val evalEvents = new BasicEventStream(new PlainTextByLineDataStream(evalReader), ",")

    val evalEventsListBuffer = new collection.mutable.ListBuffer[Event]()
    while (evalEvents.hasNext) evalEventsListBuffer.append(evalEvents.next)

    // Get and score the predictions
    val compare = evalEventsListBuffer.map { event => {
      val scores = classifier.eval(event.getContext)
      val best = scores.zipWithIndex.maxBy(_._1)._2
      (event.getOutcome, classifier.getOutcome(best))
    }}.toList

    val correct = compare.count{ case(t,p) => t == p }
    println("Accuracy: " + correct/compare.length.toDouble*100)
  }

}


/**
 * This is a second example based on the same data format, but this time
 * computing the features directly w/o using the nak.data classes. It handles
 * creating a feature index and getting the examples into the right data structures
 * for training with the logistic regression classifier, which should serve as a
 * useful example for creating features and classifiers using the API.
 * 
 * @author jasonbaldridge
 */ 
object LiblinearExample2 {

  import java.io._
  import nak.liblinear._
  import nak.util.GrowableIndex

  def main(args: Array[String]) {

    val Array(trainfile,evalfile) = args

    // Feature map
    val lmap = new GrowableIndex[String]()
    val fmap = new GrowableIndex[String]()
    fmap("DUMMY FEATURE BECAUSE LIBLINEAR STARTS WITH 1-BASED INDEX")

    // Read in the training data and index it.
    val trainingData = SparseCsvDataset(io.Source.fromFile(trainfile))
      .map { case(label, features) => {
        val indexedFeatures = features.map(fmap(_))
        val indexedLabel = lmap(label)
        (indexedLabel.toDouble, indexedFeatures)
      }}
      .toList // Need to consume the lines in order to populate the feature map

    val interceptFeature = (fmap("intercept"),1.0)

    val lmapFixed = lmap.toMap
    val fmapFixed = fmap.toMap

    val (responses, observations) = trainingData
      .map{ case(label, features) => (label, makeLibsvmFeatureVector(features) ++ List(interceptFeature)) }
      .unzip

    // Train the model
    val model = new LiblinearTrainer(1.0)(responses, observations, fmapFixed.size)
    val classifier = new LiblinearClassifier(model, lmapFixed, fmapFixed)

    // Read in the evaluation data
    val evalData = SparseCsvDataset(io.Source.fromFile(evalfile))
      .map { case(label, features) => {          
        val indexedFeatures = features.flatMap(fmapFixed.get)
        (lmapFixed(label), makeLibsvmFeatureVector(indexedFeatures) ++ List(interceptFeature))
      }}

    // Get the predictions
    val compare = evalData.map { case (label, features) => {
      val scores = classifier(features.toArray)
      val best = scores.zipWithIndex.maxBy(_._1)._2
      (label, best)
    }}.toList

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
