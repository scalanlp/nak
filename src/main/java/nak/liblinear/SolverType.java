package nak.liblinear;

import java.util.HashMap;
import java.util.Map;


public enum SolverType {

    /**
     * L2-regularized logistic regression (primal)
     *
     * (fka L2_LR)
     */
    L2R_LR(0, true, false),

    /**
     * L2-regularized L2-loss support vector classification (dual)
     *
     * (fka L2LOSS_SVM_DUAL)
     */
    L2R_L2LOSS_SVC_DUAL(1, false, false),

    /**
     * L2-regularized L2-loss support vector classification (primal)
     *
     * (fka L2LOSS_SVM)
     */
    L2R_L2LOSS_SVC(2, false, false),

    /**
     * L2-regularized L1-loss support vector classification (dual)
     *
     * (fka L1LOSS_SVM_DUAL)
     */
    L2R_L1LOSS_SVC_DUAL(3, false, false),

    /**
     * multi-class support vector classification by Crammer and Singer
     */
    MCSVM_CS(4, false, false),

    /**
     * L1-regularized L2-loss support vector classification
     *
     * @since 1.5
     */
    L1R_L2LOSS_SVC(5, false, false),

    /**
     * L1-regularized logistic regression
     *
     * @since 1.5
     */
    L1R_LR(6, true, false),

    /**
     * L2-regularized logistic regression (dual)
     *
     * @since 1.7
     */
    L2R_LR_DUAL(7, true, false),

    /**
     * L2-regularized L2-loss support vector regression (dual)
     *
     * @since 1.91
     */
    L2R_L2LOSS_SVR(11, false, true),

    /**
     * L2-regularized L1-loss support vector regression (dual)
     *
     * @since 1.91
     */
    L2R_L2LOSS_SVR_DUAL(12, false, true),

    /**
     * L2-regularized L2-loss support vector regression (primal)
     *
     * @since 1.91
     */
    L2R_L1LOSS_SVR_DUAL(13, false, true),

    ;

    private final boolean logisticRegressionSolver;
    private final boolean supportVectorRegression;
    private final int     id;

    private SolverType( int id, boolean logisticRegressionSolver, boolean supportVectorRegression ) {
        this.id = id;
        this.logisticRegressionSolver = logisticRegressionSolver;
        this.supportVectorRegression = supportVectorRegression;
    }

    private static Map<Integer, SolverType> SOLVERS_BY_ID = new HashMap<Integer, SolverType>();
    static {
        for (SolverType solverType : SolverType.values()) {
            SolverType old = SOLVERS_BY_ID.put(Integer.valueOf(solverType.getId()), solverType);
            if (old != null) throw new Error("duplicate solver type ID: " + solverType.getId());
        }
    }

    public int getId() {
        return id;
    }

    public static SolverType getById(int id) {
        SolverType solverType = SOLVERS_BY_ID.get(Integer.valueOf(id));
        if (solverType == null) {
            throw new RuntimeException("found no solvertype for id " + id);
        }
        return solverType;
    }

    /**
     * @since 1.9
     */
    public boolean isLogisticRegressionSolver() {
        return logisticRegressionSolver;
    }

    /**
     * @since 1.91
     */
    public boolean isSupportVectorRegression() {
        return supportVectorRegression;
    }
}
