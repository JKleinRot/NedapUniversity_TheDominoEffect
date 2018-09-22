package main.solver;

import java.util.List;

import main.board.Board;
import main.exception.InvalidInputException;
import main.exception.InvalidInputGridException;
import main.exception.InvalidInputSizeException;

/**
 * Solves the domino board.
 */
public interface Solver {

	/**
	 * Finds the solutions for the input.
	 * 
	 * @param input
	 *            The input
	 * @throws InvalidInputException
	 *             If the input contains other characters than the integers 0 to 6
	 * @throws InvalidInputSizeException
	 *             If the input does not have the correct size
	 * @throws InvalidInputGridException
	 *             If the input contains not all pips in the correct amounts
	 */
	public List<Board> solve(final String input)
			throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException;
}
