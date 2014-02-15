package nak.liblinear

import breeze.util.Index
import breeze.linalg.DenseMatrix

import nak.liblinear.{Function=>TronFunction, _}

/**
  * Specify a binomial problem with success/failure counts for each
  * feature vector and offsets.
  */
class BinomialProblem(
  val y: Array[(Int,Int)],      // target values
  val x: Array[Array[Feature]], // features
  val n: Int,                   // number of features
  val offsets: Array[Double]    // offsets
) {
  lazy val l: Int = y.length
}


/**
  * Companion to BinomialProblem.
  */
object BinomialProblem {

  /**
    * Construct a binomial problem with offsets equal to zero.
    */
  def apply(y: Array[(Int,Int)], x: Array[Array[Feature]], n: Int) =
    new BinomialProblem(y, x, n, Array.fill(y.length)(0.0))

}



/**
  * Train a Liblinear classifier from data with binomial success/failure counts.
  */
class LiblinearTrainerBinomial(config: LiblinearConfig) {
  
  if (!config.showDebug) Linear.disableDebugOutput
  val param = new Parameter(SolverType.L2R_LR, config.cost, config.eps)
  
  def apply(
    binomialResponses: Array[(Int,Int)],
    x: Array[Array[Feature]],
    numFeatures: Int,
    offsetsOpt: Option[Array[Double]] = None
  ): Model = {
    val problem = offsetsOpt match {
      case Some(offsets) =>
        new BinomialProblem(binomialResponses, x, numFeatures, offsets)
      case None =>
        BinomialProblem(binomialResponses, x, numFeatures)
    }
    train(problem, param)
  }

  def train (prob: BinomialProblem, param: Parameter) = {
    val w_size = prob.n
    val model = new Model
    model.solverType = param.solverType;
    model.bias = 0
    model.nr_class = 2
    model.label = Array(1,0)
    model.nr_feature = prob.n
    model.w = Array.fill(w_size)(0.0)
    train_one(prob, param, model.w, param.C, param.C)
    model
  }

  def train_one(prob: BinomialProblem, param: Parameter, w: Array[Double], Cp: Double, Cn: Double) {
    val eps = param.eps
    val pos = prob.y.map(_._1).sum
    val neg = prob.y.map(_._2).sum
    val primal_solver_tol = eps * math.max(math.min(pos, neg), 1) / (pos+neg)
    val fun_obj = new L2R_LrFunction_Binomial(prob, Cp, Cn)
    new Tron(fun_obj, primal_solver_tol).tron(w)
  }

}

/**
  * Binomial logistic regression function with offsets.
  *
  * This is horrifically mutable code, but is made to line up with how
  * other liblinear functions were done in Java. Also, did the while-loop
  * thing to iterate over arrays since it is a bit more efficient than
  * equivalent for-loops in Scala.
  */ 
class L2R_LrFunction_Binomial(prob: BinomialProblem, Cp: Double, Cn: Double)
    extends TronFunction {

  private val l = prob.l
  private val z = Array.fill(l)(0.0)
  private val D = Array.fill(l)(0.0)

  private def Xv(v: Array[Double], Xv: Array[Double]) {
    var i = 0
    while (i < prob.l) {
      Xv(i) = prob.offsets(i)
      for (s <- prob.x(i))
        Xv(i) += v(s.getIndex - 1) * s.getValue
      i += 1
    }
  }

  private def XTv(v: Array[Double], XTv: Array[Double]) {
    val w_size = get_nr_variable
    val x = prob.x

    var i = 0
    while (i < w_size) {
      XTv(i) = 0.0
      i += 1
    }

    i = 0
    while (i < l) {
      val v_i = v(i)
      for (s <- x(i))
        XTv(s.getIndex-1) += v_i * s.getValue
      i += 1
    }
  }

  // Calculate log-likelihood
  def fun(w: Array[Double]): Double = {
    val y = prob.y
    val w_size = get_nr_variable

    Xv(w, z)

    var i = 0

    var f = 0.0
    while (i < w_size) {
      val p = w(i)
      f += p*p
      i += 1
    }
    f /= 2.0

    i = 0
    while (i < l) {
      val (numPosObs, numNegObs) = y(i)
      val logOnePlusZi = math.log(1+math.exp(z(i)))
      f += (if (numPosObs > 0) numPosObs*(logOnePlusZi - z(i)) else 0.0)
      f += (if (numNegObs > 0) numNegObs*logOnePlusZi else 0.0)
      i += 1
    }
    f
  }

  // Calculate gradient
  def grad(w: Array[Double], g: Array[Double]) {
    val y = prob.y;
    val w_size = get_nr_variable

    var i = 0
    while (i < l) {
      val (numPosObs, numNegObs) = y(i)
      val numTrials = numPosObs + numNegObs
      val zPos = 1 / (1 + math.exp(-z(i)))
      D(i) = numTrials  * zPos * (1-zPos)
      z(i) = -(numPosObs - numTrials*zPos)
      i += 1
    }
    XTv(z, g)

    i = 0
    while (i < w_size) {
      g(i) += w(i)
      i += 1
    }
  }

  def Hv(s: Array[Double], Hs: Array[Double]) {
    val w_size = get_nr_variable
    val wa = Array.fill(l)(0.0)

    Xv(s, wa)

    var i = 0
    while (i < l) {
      val (numPosObs, numNegObs) = prob.y(i)
      wa(i) = numPosObs * Cp * D(i) * wa(i) + numNegObs * Cn * D(i) * wa(i)
      i += 1
    }

    XTv(wa, Hs)

    i = 0
    while (i < w_size) {
      Hs(i) += s(i)
      i += 1
    }
  }

  lazy val get_nr_variable = prob.n

}

