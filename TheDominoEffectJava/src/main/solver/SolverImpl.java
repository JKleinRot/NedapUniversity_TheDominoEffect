package main.solver;

import java.util.List;
import java.util.Map;

import main.board.Board;
import main.board.Dominos;
import main.board.Dominos.Domino;
import main.board.Board.PairOfIndices;
import main.exception.InvalidInputException;
import main.exception.InvalidInputGridException;
import main.exception.InvalidInputSizeException;

public class SolverImpl implements Solver {

	/** The board */
	private Board board;

	/** The dominos */
	private Dominos dominos;

	/**
	 * Constructor.
	 */
	public SolverImpl() {
		dominos = new Dominos();
	}

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
	public void solve(final String input)
			throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException {
		createBoard(input);
		Map<Domino, List<PairOfIndices>> matchingIndices = board.findMatchingIndices(dominos);
	}

	/**
	 * Creates the domino board from the input.
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
	private void createBoard(final String input)
			throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException {
		board = new Board(input);
	}

}
