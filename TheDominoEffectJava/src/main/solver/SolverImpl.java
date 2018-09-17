package main.solver;

import main.board.Board;
import main.exception.InvalidInputException;
import main.exception.InvalidInputGridException;
import main.exception.InvalidInputSizeException;

public class SolverImpl implements Solver {
	
	/** The board */
	private Board board;
	
	/**
	 * Constructor.
	 */
	public SolverImpl() {
		
	}
	
	public void solve(final String input) throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException {
		createBoard(input);
	}
	
	private void createBoard(final String input) throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException {
		board = new Board(input);
	}

}
