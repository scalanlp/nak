package nak.liblinear

class LiblinearConfig(
  val solverType: SolverType = SolverType.L2R_LR,
  val cost: Double = 1.0, 
  val eps: Double = 0.01, 
  val showDebug: Boolean = false)

