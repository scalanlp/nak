package nak.app

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


import nak.NakContext._
import nak.core._
import nak.data._
import nak.liblinear.{LiblinearConfig,SolverType}
import nak.util.{GrowableIndex, ConfusionMatrix}
import java.io._

/**
 * An application that uses the API to perform classification on files with
 * features already extracted in CSV format, e.g. run the following:
 *
 * $ bin/nak classify -t data/classify/tennis/train --eval data/classify/tennis/test
 *
 * A confusion matrix is output if requested.
 *
 * @author jasonbaldridge
 */
object Classify {

  def main(args: Array[String]) {

    // Parse and get the command-line options.
    val opts = ClassifyOpts(args)

    // Choose the solver.
    val solverType = nak.liblinear.Solver(opts.solverType())

    // Read and index the examples in the training file.
    val indexer = new ExampleIndexer
    val examples = fromCsvFile(opts.trainfile()).toList.map(indexer)
    val (lmap,fmap) = indexer.getMaps
        
    // Configure and train with liblinear.
    val config = new LiblinearConfig(solverType, opts.cost(), opts.eps(), opts.verbose())
    val classifier = LiblinearTrainer.train(examples, lmap, fmap, config)

    // Evaluate if requested.
    if (opts.evalfile.get != None) {

      val evalData = fromCsvFile(opts.evalfile()).toList

      // Get the output distributions for the evaluation data.
      val comparisons = for (ex <- evalData) yield {
	val scores = classifier.evalRaw(ex.features)
	val best = scores.zipWithIndex.maxBy(_._1)._2
	(ex.label, classifier.getOutcome(best))
      }

      // Compute and print out the confusion matrix.
      val (goldLabels, predictions) = comparisons.unzip
      val inputs = evalData.map(_.features.map(_.feature).mkString(" "))
      val cmatrix = ConfusionMatrix(goldLabels, predictions, inputs)
      println(cmatrix)
    }
  }
}

/**
 * An object that sets up the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 *
 */
object ClassifyOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
Classification application.

For usage see below (nb: some options are currently not supported).
	     """)

    val solverTypes = nak.liblinear.Solver.solverTypes
    val solverType = opt[String](
      "solverType", default=Some("L2R_LR"), validate=solverTypes, 
      descr="The type of solver to use. Possible values: " 
      + solverTypes.toSeq.sorted.mkString(",") 
      + ". See the following for descriptions of each: https://github.com/bwaldvogel/liblinear-java")

    val cost = opt[Double]("cost", default=Some(1.0), validate = (0<), descr="The cost parameter C. Bigger values means less regularization (more fidelity to the training set).")

    val eps = opt[Double]("eps", default=Some(0.01), validate = (0<), descr="The tolerance of the stopping criterion. Smaller values mean the parameters found will be better wrt the objective function, but will also entail longer training times (and won't necessarily improve performance).")

    val trainfile = opt[String]("train", required=true,descr="The file containing training events.")

    val evalfile = opt[String]("eval", noshort=true, descr="The file containing evalualation events.")

    val predictfile = opt[String]("predictfile", descr="The file to save the predictions of the evaluation events.")

    val modelfile = opt[String]("modelfile", descr="The file to save the model as.")

    val help = opt[Boolean]("help", noshort = true, descr="Show this message")

    val verbose = opt[Boolean]("verbose")
  }
}
