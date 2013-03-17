package nak.liblinear;


final class DoubleArrayPointer {

    private final double[] _array;
    private int            _offset;


    public void setOffset(int offset) {
        if (offset < 0 || offset >= _array.length) throw new IllegalArgumentException("offset must be between 0 and the length of the array");
        _offset = offset;
    }

    public DoubleArrayPointer( final double[] array, final int offset ) {
        _array = array;
        setOffset(offset);
    }

    public double get(final int index) {
        return _array[_offset + index];
    }

    public void set(final int index, final double value) {
        _array[_offset + index] = value;
    }
}
