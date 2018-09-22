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

/**
 * Implementation of the Solver.
 */
public class SolverImpl implements Solver {

	/** The solutions */
	private final List<Board> solutions;

	/**
	 * Constructor.
	 */
	public SolverImpl() {
		solutions = new ArrayList<>();
	}

	@Override
	public List<Board> solve(final String input)
			throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException {
		final Board board = createBoard(input);
		final Dominos dominos = new Dominos();
		solutions.add(board.clone());
		solveBoard(board, dominos);
		return solutions;
	}
	
	/**
	 * Returns the domino board created from the input.
	 * 
	 * @param input
	 *            The input
	 * @return The new board
	 * @throws InvalidInputException
	 *             If the input contains other characters than the integers 0 to 6
	 * @throws InvalidInputSizeException
	 *             If the input does not have the correct size
	 * @throws InvalidInputGridException
	 *             If the input contains not all pips in the correct amounts
	 */
	private Board createBoard(final String input)
			throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException {
		return new Board(input);
	}

	/**
	 * Returns the dominos which can be placed on more than one index and the
	 * corresponding indices.
	 * 
	 * @param matchingIndices
	 *            The matching indices
	 * @return The dominos and indices for more fit dominos
	 */
	private Map<Domino, List<PairOfIndices>> getMoreFitDominos(final Map<Domino, List<PairOfIndices>> matchingIndices) {
		return matchingIndices.entrySet().stream().filter(i -> i.getValue().size() > 1)
				.collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue()));
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
	 * Solves the board for the list of available dominos.
	 * 
	 * @param board
	 *            The board
	 * @param dominos
	 *            The available dominos
	 */
	private void solveBoard(final Board board, final Dominos dominos) {
		if (dominos.isEmpty() && board.isFilled()) {
			solutions.add(board);
			return;
		}
		final Map<Domino, List<PairOfIndices>> matchingIndices = board.findMatchingIndices(dominos);
		if (hasUnplacableDominos(matchingIndices)) {
			return;
		}
		final Map<Domino, PairOfIndices> oneFitDominos = getOneFitDominos(matchingIndices);
		if (oneFitDominos.size() > 0) {
			for (final Map.Entry<Domino, PairOfIndices> domino : oneFitDominos.entrySet()) {
				board.placeDomino(domino.getKey(), domino.getValue());
				dominos.removeDomino(domino.getKey());
			}
			solveBoard(board, dominos);
		} else {
			final Map<Domino, List<PairOfIndices>> moreFitDominos = getMoreFitDominos(matchingIndices);
			if (!moreFitDominos.isEmpty()) {
				final Domino moreFitDomino = (Domino) moreFitDominos.keySet().toArray()[0];
				for (final PairOfIndices pair : moreFitDominos.get(moreFitDomino)) {
					final Board newBoard = board.clone().placeDomino(moreFitDomino, pair);
					final Dominos newDominos = dominos.clone().removeDomino(moreFitDomino);
					solveBoard(newBoard, newDominos);
				}
			} else if (board.isFilled()) {
				solutions.add(board);
			}
		}
	}
}
