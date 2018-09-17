package main.board;

import main.exception.InvalidInputException;
import main.exception.InvalidInputGridException;
import main.exception.InvalidInputSizeException;

/**
 * The board on which the dominos can be placed.
 */
public class Board {
	
	/** The positions */
	private final Position[][] positions;
	
	public Board (final String input) throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException {
		if (!input.matches("[0-6]+")) {
			throw new InvalidInputException("Input contains characters other than the integers 0 to 6");
		}
		if (input.length() != 56) {
			throw new InvalidInputSizeException("Input has not the correct size");
		}
		if (input.chars().filter(num -> Character.getNumericValue(num) == 0).count() != 8 || 
				input.chars().filter(num -> Character.getNumericValue(num) == 1).count() != 8 || 
				input.chars().filter(num -> Character.getNumericValue(num) == 2).count() != 8 || 
				input.chars().filter(num -> Character.getNumericValue(num) == 3).count() != 8 || 
				input.chars().filter(num -> Character.getNumericValue(num) == 4).count() != 8 || 
				input.chars().filter(num -> Character.getNumericValue(num) == 5).count() != 8 ||
				input.chars().filter(num -> Character.getNumericValue(num) == 6).count() != 8) {
			throw new InvalidInputGridException("Input grid is not valid, does not contain correct amount of each pip");
		}
		positions = new Position[8][7];
		for (int y = 0; y < 7; y++) {
			for (int x = 0; x < 8; x++) {
				positions[x][y] = new Position(Character.getNumericValue(input.charAt(y*8 + x)));
			}
		}
	}
}
