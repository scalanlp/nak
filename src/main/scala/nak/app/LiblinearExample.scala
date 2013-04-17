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
@deprecated(message="This uses the legacy Nak API. See nak.app.Classify and nak.example.PpaExample for use of the new API.", since="1.1.2")
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
    val config = new LiblinearConfig()
    val classifierTrained = LiblinearTrainer.trainLegacy(config, dataIndexer)

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
