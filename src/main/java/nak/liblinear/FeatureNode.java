package nak.liblinear;

public class FeatureNode implements Feature {

    public final int index;
    public double    value;

    public FeatureNode( final int index, final double value ) {
        if (index < 0) throw new IllegalArgumentException("index must be >= 0");
        this.index = index;
        this.value = value;
    }

    /**
     * @since 1.9
     */
    public int getIndex() {
        return index;
    }

    /**
     * @since 1.9
     */
    public double getValue() {
        return value;
    }

    /**
     * @since 1.9
     */
    public void setValue(double value) {
        this.value = value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + index;
        long temp;
        temp = Double.doubleToLongBits(value);
        result = prime * result + (int)(temp ^ (temp >>> 32));
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        FeatureNode other = (FeatureNode)obj;
        if (index != other.index) return false;
        if (Double.doubleToLongBits(value) != Double.doubleToLongBits(other.value)) return false;
        return true;
    }

    @Override
    public String toString() {
        return "FeatureNode(idx=" + index + ", value=" + value + ")";
    }
}
