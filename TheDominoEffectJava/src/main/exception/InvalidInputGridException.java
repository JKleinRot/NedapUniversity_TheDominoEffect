package main.exception;

/**
 * Exception thrown if the input is invalid due to the input not having equal
 * amounts of each pip.
 */
public class InvalidInputGridException extends Exception {

	/** The serial version UID */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor
	 * 
	 * @param message
	 *            The message
	 */
	public InvalidInputGridException(String message) {
		super(message);
	}
}
