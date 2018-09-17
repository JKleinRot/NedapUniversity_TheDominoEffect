package main.exception;

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
