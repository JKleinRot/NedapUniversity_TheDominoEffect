package main.exception;

/**
 * Exception thrown if the input is invalid due to invalid characters.
 */
public class InvalidInputException extends Exception {

	/** The serial version UID */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor.
	 * 
	 * @param message
	 *            The message
	 */
	public InvalidInputException(String message) {
		super(message);
	}
}
