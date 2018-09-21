package main.solver;

import java.util.List;

import main.board.Board;
import main.exception.InvalidInputException;
import main.exception.InvalidInputGridException;
import main.exception.InvalidInputSizeException;

public interface Solver {

	/**
	 * Solves the domino effect problem.
	 * 
	 * @param input
	 *            The input grid
	 * @return The solutions
	 */
	public List<Board> solve(final String input)
			throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException;
}
