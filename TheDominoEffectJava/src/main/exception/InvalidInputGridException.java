package main.exception;

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
