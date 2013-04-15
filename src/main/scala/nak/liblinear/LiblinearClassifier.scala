package nak.liblinear

class LiblinearConfig(
  val solverType: SolverType = SolverType.L2R_LR,
  val cost: Double = 1.0, 
  val eps: Double = 0.01, 
  val showDebug: Boolean = false)


object Solver {
 
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
