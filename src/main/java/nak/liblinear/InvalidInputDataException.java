package nak.liblinear;

import java.io.File;


public class InvalidInputDataException extends Exception {

    private static final long serialVersionUID = 2945131732407207308L;

    private final int         _line;

    private File              _file;

    public InvalidInputDataException( String message, File file, int line ) {
        super(message);
        _file = file;
        _line = line;
    }

    public InvalidInputDataException( String message, String filename, int line ) {
        this(message, new File(filename), line);
    }

    public InvalidInputDataException( String message, File file, int lineNr, Exception cause ) {
        super(message, cause);
        _file = file;
        _line = lineNr;
    }

    public InvalidInputDataException( String message, String filename, int lineNr, Exception cause ) {
        this(message, new File(filename), lineNr, cause);
    }

    public File getFile() {
        return _file;
    }

    /**
     * This methods returns the path of the file.
     * The method name might be misleading.
     *
     * @deprecated use {@link #getFile()} instead
     */
    public String getFilename() {
        return _file.getPath();
    }

    public int getLine() {
        return _line;
    }

    @Override
    public String toString() {
        return super.toString() + " (" + _file + ":" + _line + ")";
    }

}
