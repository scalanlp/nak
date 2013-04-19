package nak.liblinear

/**
 * Configure the options for Liblinear training.
 */
case class LiblinearConfig(
  solverType: SolverType = SolverType.L2R_LR,
  cost: Double = 1.0, 
  eps: Double = 0.01, 
  showDebug: Boolean = false)

/**
 * Set up a problem to be solved.
 */ 
object LiblinearProblem {

  def apply(responses: Array[Double], observations: Array[Array[Feature]], numFeats: Int) = {
    val problem = new Problem
    problem.y = responses
    problem.x = observations
    problem.l = responses.length
    problem.n = numFeats
    problem
  }

}

/**
 * An object to help with solver descriptions.
 */
object Solver {

  /**
   * The set of all valid solver types.
   */
  lazy val solverTypes = Set(
    "L2R_LR", "L2R_L2LOSS_SVC_DUAL", "L2R_L2LOSS_SVC", "L2R_L1LOSS_SVC_DUAL",
    "MCSVM_CS","L1R_L2LOSS_SVC", "L1R_LR",
    "L2R_LR_DUAL", "L2R_L2LOSS_SVR", "L2R_L2LOSS_SVR_DUAL", "L2R_L1LOSS_SVR_DUAL")

  /**
   * Select the right solver given the textual description.
   */
  def apply(solverDescription: String) = solverDescription match {
    case "L2R_LR" => SolverType.L2R_LR
    case "L2R_L2LOSS_SVC_DUAL" => SolverType.L2R_L2LOSS_SVC_DUAL
    case "L2R_L2LOSS_SVC" => SolverType.L2R_L2LOSS_SVC
    case "L2R_L1LOSS_SVC_DUAL" => SolverType.L2R_L1LOSS_SVC_DUAL
    case "MCSVM_CS" => SolverType.MCSVM_CS
    case "L1R_L2LOSS_SVC" => SolverType.L1R_L2LOSS_SVC
    case "L1R_LR" => SolverType.L1R_LR
    case "L2R_LR_DUAL" => SolverType.L2R_LR_DUAL
    case "L2R_L2LOSS_SVR" => SolverType.L2R_L2LOSS_SVR
    case "L2R_L2LOSS_SVR_DUAL" => SolverType.L2R_L2LOSS_SVR_DUAL
    case "L2R_L1LOSS_SVR_DUAL" => SolverType.L2R_L1LOSS_SVR_DUAL 
    case invalid => throw new MatchError("No solver with the name " + invalid)
  }

}

/**
 * Helper functions for working with Liblinear.
 */
object LiblinearUtil {


  /**
   * Convert tuples into Liblinear Features, basically.
   */ 
  def createLiblinearMatrix(observations: Seq[Seq[(Int,Double)]]): Array[Array[Feature]] =  
    observations.map { features =>
      features.map{ case(a,v) => new FeatureNode(a,v).asInstanceOf[Feature] }.toArray
    }.toArray

  /**
   * Convert tuples into Liblinear Features, basically.
   *
   * TODO: Condense so there is just one createLiblinearMatrix.
   */ 
  def createLiblinearMatrix(observations: Array[Array[(Int,Float)]]): Array[Array[Feature]] =  
    observations.map { features => {
      features
        .sortBy(_._1)
        .map{ case(a,v) => new FeatureNode(a,v).asInstanceOf[Feature] }
    }}

}

/**
 * Train a Liblinear classifier from data.
 * 
 * @author jasonbaldridge
 */
class LiblinearTrainer(config: LiblinearConfig) {

  import LiblinearUtil._

  if (!config.showDebug) Linear.disableDebugOutput
  val param = new Parameter(config.solverType, config.cost, config.eps)

  /**
   * Train a liblinear model given the responses (the y's), the observations (the x's),
   * and the number of features.
   */ 
  def apply(
    responses: Array[Double], 
    observations: Array[Array[Feature]], 
    numFeatures: Int
  ): Model = {
    val problem = LiblinearProblem(responses, observations, numFeatures)
    Linear.train(problem, param)
  }

}

