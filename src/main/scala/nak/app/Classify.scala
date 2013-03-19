package nak.app

object Classify {

  import java.io._
  import nak.core._
  import nak.data._
  import nak.io._
  import nak.liblinear._
  import nak.maxent._

  def main(args: Array[String]) {

    // Parse and get the command-line options
    val opts = ClassifyOpts(args)
    
    val trainingReader = new BufferedReader(new FileReader(opts.trainfile()))
    val trainingEvents = 
      new BasicEventStream(new PlainTextByLineDataStream(trainingReader), ",")
    
    val classifier = opts.solverType() match {
      case "GIS" =>
        GIS.trainModel(trainingEvents, opts.maxit(), opts.cutoff(), opts.cost(), opts.verbose())

      case liblinearSolverType =>
	val solverType = liblinearSolverType match {
	  case "L2R_LR" => SolverType.L2R_LR
	  case "L1R_LR" => SolverType.L1R_LR
	}
	val config = new LiblinearConfig(solverType, opts.cost(), opts.eps(), opts.verbose())
	val indexer = new OnePassDataIndexer(trainingEvents, opts.cutoff(), true, opts.verbose())
	LiblinearTrainer.train(indexer, config)

    }

    // Save the model if a model file is specified.
    if (opts.modelfile.get != None)
      new GenericModelWriter(classifier, new File(opts.modelfile())).persist
    
    if (opts.evalfile.get != None) {
      // Read in the evaluation data
      val evalReader = new BufferedReader(new FileReader(opts.evalfile()))
      val evalEvents = new BasicEventStream(new PlainTextByLineDataStream(evalReader), ",")

      val evalEventsListBuffer = new collection.mutable.ListBuffer[Event]()
      while (evalEvents.hasNext) evalEventsListBuffer.append(evalEvents.next)

      // Get and score the predictions
      val predictions =  evalEventsListBuffer.map { event => {
	val scores = classifier.eval(event.getContext)
	(event.getOutcome, scores)
      }}.toList

      val correct = predictions.count { case(trueLabel, scores) => {
	val best = scores.zipWithIndex.maxBy(_._1)._2
	trueLabel == classifier.getOutcome(best)
      }}

      println("Accuracy: " + correct/predictions.length.toDouble*100)

      if (opts.predictfile.get != None) {
	val output = new BufferedWriter(new FileWriter(opts.predictfile()))
	val labels = (0 until classifier.getNumOutcomes).map(classifier.getOutcome).toArray
	predictions.foreach { case(_, scores) => {
	  val sortedScores = labels.zip(scores).sortBy(-_._2)
	  output.write(sortedScores.map(ls => ls._1 + " " + ls._2).mkString(" ") + "\n")
	}}
	output.close
      }
      
    }
  }
}


/**
 * An object that sets up the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object ClassifyOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
Classification application.

For usage see below:
	     """)

    val solverTypes = Set("GIS","L2R_LR","L1R_LR")
    val solverType = opt[String]("solverType", default=Some("L2R_LR"), validate = solverTypes, descr="The type of solver to use. Possible values: " + solverTypes.toSeq.sorted.mkString(",") )

    val maxit = opt[Int]("maxit", default=Some(100), validate = (0<), descr="The maximum number of iterations to run (only used for the GIS solver).")

    val cutoff = opt[Int]("cutoff", noshort=true, default=Some(1), validate = (0<), descr="The minumum number of times a feature must be seen. Any that occur less than the cutoff value will be ignored.")

    val cost = opt[Double]("cost", default=Some(1.0), validate = (0<), descr="The cost parameter C. Bigger values means less regularization (more fidelity to the training set). Note: if you are using the GIS solver, this option instead indicates the standard deviation of the Gaussian penalty (bigger values still mean less regularization).")

    val eps = opt[Double]("eps", default=Some(0.01), validate = (0<), descr="The tolerance of the stopping criterion. Smaller values mean the parameters found will be better wrt the objective function, but will also entail longer training times (and won't necessarily improve performance).")

    val trainfile = opt[String]("train", required=true,descr="The file containing training events.")

    val evalfile = opt[String]("eval", noshort=true, descr="The file containing evalualation events.")

    val predictfile = opt[String]("predictfile", descr="The file to save the predictions of the evaluation events.")

    val modelfile = opt[String]("modelfile", descr="The file to save the model as.")

    val help = opt[Boolean]("help", noshort = true, descr="Show this message")

    val verbose = opt[Boolean]("verbose")
  }
}
