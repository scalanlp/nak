package nak.liblinear;

class L2R_LrFunction implements Function {

    private final double[] C;
    private final double[] z;
    private final double[] D;
    private final Problem  prob;

    public L2R_LrFunction( Problem prob, double[] C ) {
        int l = prob.l;

        this.prob = prob;

        z = new double[l];
        D = new double[l];
        this.C = C;
    }


    private void Xv(double[] v, double[] Xv) {

        for (int i = 0; i < prob.l; i++) {
            Xv[i] = 0;
            for (Feature s : prob.x[i]) {
                Xv[i] += v[s.getIndex() - 1] * s.getValue();
            }
        }
    }

    private void XTv(double[] v, double[] XTv) {
        int l = prob.l;
        int w_size = get_nr_variable();
        Feature[][] x = prob.x;

        for (int i = 0; i < w_size; i++)
            XTv[i] = 0;

        for (int i = 0; i < l; i++) {
            for (Feature s : x[i]) {
                XTv[s.getIndex() - 1] += v[i] * s.getValue();
            }
        }
    }


    public double fun(double[] w) {
        int i;
        double f = 0;
        double[] y = prob.y;
        int l = prob.l;
        int w_size = get_nr_variable();

        Xv(w, z);

        for (i = 0; i < w_size; i++)
            f += w[i] * w[i];
        f /= 2.0;
        for (i = 0; i < l; i++) {
            double yz = y[i] * z[i];
            if (yz >= 0)
                f += C[i] * Math.log(1 + Math.exp(-yz));
            else
                f += C[i] * (-yz + Math.log(1 + Math.exp(yz)));
        }

        return (f);
    }

    public void grad(double[] w, double[] g) {
        int i;
        double[] y = prob.y;
        int l = prob.l;
        int w_size = get_nr_variable();

        for (i = 0; i < l; i++) {
            z[i] = 1 / (1 + Math.exp(-y[i] * z[i]));
            D[i] = z[i] * (1 - z[i]);
            z[i] = C[i] * (z[i] - 1) * y[i];
        }
        XTv(z, g);

        for (i = 0; i < w_size; i++)
            g[i] = w[i] + g[i];
    }

    public void Hv(double[] s, double[] Hs) {
        int i;
        int l = prob.l;
        int w_size = get_nr_variable();
        double[] wa = new double[l];

        Xv(s, wa);
        for (i = 0; i < l; i++)
            wa[i] = C[i] * D[i] * wa[i];

        XTv(wa, Hs);
        for (i = 0; i < w_size; i++)
            Hs[i] = s[i] + Hs[i];
        // delete[] wa;
    }

    public int get_nr_variable() {
        return prob.n;
    }

}
