package main.exception;

public class InvalidInputException extends Exception {

	/** The serial version UID */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor
	 * 
	 * @param message
	 *            The message
	 */
	public InvalidInputException(String message) {
		super(message);
	}
}
