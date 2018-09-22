package main.exception;

/**
 * Exception thrown in the input is invalid due to the input not having the
 * correct amount of characters.
 * 
 * @author janine.kleinrot
 *
 */
public class InvalidInputSizeException extends Exception {

	/** 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor.
	 * 
	 * @param message
	 *            The message
	 */
	public InvalidInputSizeException(final String message) {
		super(message);
	}
}
