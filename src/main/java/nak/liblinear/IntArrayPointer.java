package nak.liblinear;


final class IntArrayPointer {

    private final int[] _array;
    private int         _offset;


    public void setOffset(int offset) {
        if (offset < 0 || offset >= _array.length) throw new IllegalArgumentException("offset must be between 0 and the length of the array");
        _offset = offset;
    }

    public IntArrayPointer( final int[] array, final int offset ) {
        _array = array;
        setOffset(offset);
    }

    public int get(final int index) {
        return _array[_offset + index];
    }

    public void set(final int index, final int value) {
        _array[_offset + index] = value;
    }
}
