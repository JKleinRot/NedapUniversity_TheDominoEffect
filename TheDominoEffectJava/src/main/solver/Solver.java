package main.solver;

import main.exception.InvalidInputException;
import main.exception.InvalidInputGridException;
import main.exception.InvalidInputSizeException;

public interface Solver {

	/**
	 * Solves the domino effect problem.
	 * 
	 * @param input
	 *            The input grid
	 */
	public void solve(final String input) throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException;
}
