package main.solver;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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
		List<Board> = solveBoard();
		Map<Domino, List<PairOfIndices>> matchingIndices = board.findMatchingIndices(dominos);
	}

	private List<Board> solveBoard() {
		List<Board> solutions = new ArrayList<>();
		Map<Domino, List<PairOfIndices>> matchingIndices = board.findMatchingIndices(dominos);
		if (hasUnplacableDominos(matchingIndices)) {
			return new ArrayList<>();
		}
		Map<Domino, PairOfIndices> oneFitDominos = getOneFitDominos(matchingIndices);
		if (oneFitDominos.size() > 0) {
			for (Map.Entry<Domino, PairOfIndices> domino : oneFitDominos.entrySet()) {
				placeDomino(domino.getKey(), domino.getValue());
			}
		}
	}

	/**
	 * Places the domino on the board.
	 * 
	 * @param domino
	 *            The domino
	 * @param indices
	 *            The indices
	 */
	private void placeDomino(final Domino domino, final PairOfIndices indices) {
		board = board.placeDomino(domino, indices);
	}

	/**
	 * Whether or not one or more of the dominos is unplacable on the board.
	 * 
	 * @param matchingIndices
	 *            The matching indices
	 * @return Whether or not one or more or the dominos is unplacable
	 */
	private boolean hasUnplacableDominos(final Map<Domino, List<PairOfIndices>> matchingIndices) {
		return matchingIndices.entrySet().stream().filter(i -> i.getValue().size() == 0)
				.collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue())).size() > 0;
	}

	/**
	 * Returns the dominos which can be placed on only one index and the
	 * corresponding indices.
	 * 
	 * @param matchingIndices
	 *            The matching indices
	 * @return The dominos and indices for one fit dominos
	 */
	private Map<Domino, PairOfIndices> getOneFitDominos(final Map<Domino, List<PairOfIndices>> matchingIndices) {
		return matchingIndices.entrySet().stream().filter(i -> i.getValue().size() == 1)
				.collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue().get(0)));
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
