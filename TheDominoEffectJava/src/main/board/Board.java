package main.board;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import main.board.Dominos.Domino;
import main.exception.InvalidInputException;
import main.exception.InvalidInputGridException;
import main.exception.InvalidInputSizeException;

/**
 * The board on which the dominos can be placed.
 */
public class Board {

	/** The positions */
	private final Position[][] positions;

	/**
	 * Constructor.
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
	public Board(final String input)
			throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException {
		if (!input.matches("[0-6]+")) {
			throw new InvalidInputException("Input contains characters other than the integers 0 to 6");
		}
		if (input.length() != 56) {
			throw new InvalidInputSizeException("Input does not have the correct size");
		}
		if (input.chars().filter(num -> Character.getNumericValue(num) == 0).count() != 8
				|| input.chars().filter(num -> Character.getNumericValue(num) == 1).count() != 8
				|| input.chars().filter(num -> Character.getNumericValue(num) == 2).count() != 8
				|| input.chars().filter(num -> Character.getNumericValue(num) == 3).count() != 8
				|| input.chars().filter(num -> Character.getNumericValue(num) == 4).count() != 8
				|| input.chars().filter(num -> Character.getNumericValue(num) == 5).count() != 8
				|| input.chars().filter(num -> Character.getNumericValue(num) == 6).count() != 8) {
			throw new InvalidInputGridException("Input grid is not valid, does not contain correct amount of each pip");
		}
		positions = new Position[7][8];
		for (int x = 0; x < 7; x++) {
			for (int y = 0; y < 8; y++) {
				positions[x][y] = new Position(Character.getNumericValue(input.charAt(x * 8 + y)));
			}
		}
	}

	/**
	 * Constructor for copying.
	 * 
	 * @param positions
	 *            The current positions
	 */
	private Board(final Position[][] positions) {
		this.positions = positions;
	}

	/**
	 * Returns a cloned board.
	 * 
	 * @return The cloned board
	 */
	public Board clone() {
		final Position[][] copiedPositions = new Position[7][8];
		for (int x = 0; x < 7; x++) {
			for (int y = 0; y < 8; y++) {
				copiedPositions[x][y] = positions[x][y];
			}
		}
		return new Board(copiedPositions);
	}

	public Position getPosition(final Index index) {
		return positions[index.getRow()][index.getColumn()];
	}
	
	public boolean isFilled() {
		for (int x = 0; x < 7; x++) {
			for (int y = 0; y < 8; y++) {
				if (!positions[x][y].isOccupied()) {
					return false;
				}
			}
		}
		return true;
	}
	
	public void showPips() {
		StringBuilder builder = new StringBuilder();
		for (int x = 0; x < 7; x++) {
			for (int y = 0; y < 8; y++) {
				builder.append("   " + positions[x][y].getPip());
				if (y == 7) {
					builder.append("\n");
				}
			}
		}
		System.out.println(builder.toString());
	}
	
	public void showBones() {
		StringBuilder builder = new StringBuilder();
		for (int x = 0; x < 7; x++) {
			for (int y = 0; y < 8; y++) {
				if (positions[x][y].getBone() < 10) {
					builder.append("   " + positions[x][y].getBone()); 
				} else {
					builder.append("  " + positions[x][y].getBone());
				}
				if (y == 7) {
					builder.append("\n");
				}
			}
		}
		System.out.println(builder.toString());
	}

	/**
	 * Finds the matching indices on the board for each of the dominos.
	 * 
	 * @param dominos
	 *            The dominos
	 * @return The matching indices
	 */
	public Map<Domino, List<PairOfIndices>> findMatchingIndices(final Dominos dominos) {
		Map<Domino, List<Index>> matchingIndicesFirstPip = findMatchingIndicesFirstPip(dominos);
		return removeDuplicates(findMatchingPairsOfIndices(dominos, matchingIndicesFirstPip));
	}

	/**
	 * Places the domino on the board.
	 * 
	 * @param domino
	 *            The domino
	 * @param indices
	 *            The indices
	 * @return The resulting board
	 */
	public Board placeDomino(final Domino domino, final PairOfIndices indices) {
		Position first = positions[indices.getFirstIndex().getRow()][indices.getFirstIndex().getColumn()];
		Position second = positions[indices.getSecondIndex().getRow()][indices.getSecondIndex().getColumn()];
		positions[indices.getFirstIndex().getRow()][indices.getFirstIndex().getColumn()] = first
				.withBone(domino.getBone());
		positions[indices.getSecondIndex().getRow()][indices.getSecondIndex().getColumn()] = second
				.withBone(domino.getBone());
		return this;
	}

	/**
	 * Finds the matching indices on the board for the first pip of each of the
	 * dominos.
	 * 
	 * @param dominos
	 *            The dominos
	 * @return The matching indices of the first pip
	 */
	private Map<Domino, List<Index>> findMatchingIndicesFirstPip(final Dominos dominos) {
		Map<Domino, List<Index>> firstPipIndices = new HashMap<>();
		for (Domino domino : dominos.getDominos()) {
			int firstPip = domino.getPips().getFirstPip();
			List<Index> matchingIndices = new ArrayList<>();
			for (int x = 0; x < 7; x++) {
				for (int y = 0; y < 8; y++) {
					if (!positions[x][y].isOccupied() && positions[x][y].getPip() == firstPip) {
						matchingIndices.add(new Index(x, y));
					}
				}
			}
			firstPipIndices.put(domino, matchingIndices);
		}
		return firstPipIndices;
	}

	/**
	 * Finds the matching pairs of indices for the list of dominos
	 * 
	 * @param dominos
	 *            The dominos
	 * @param matchingIndicesFirstPip
	 *            the indices for the first pips
	 * @return The matching pairs of indices
	 */
	private Map<Domino, List<PairOfIndices>> findMatchingPairsOfIndices(final Dominos dominos,
			final Map<Domino, List<Index>> matchingIndicesFirstPip) {
		Map<Domino, List<PairOfIndices>> matchingNeighboursOfIndices = new HashMap<>();
		for (Map.Entry<Domino, List<Index>> matchingIndexFirstPip : matchingIndicesFirstPip.entrySet()) {
			int secondPip = matchingIndexFirstPip.getKey().getPips().getSecondPip();
			List<PairOfIndices> matchingNeighboursOfIndex = new ArrayList<>();
			for (Index index : matchingIndexFirstPip.getValue()) {
				List<Index> neighboursOfIndex = findNeighboursOfIndex(index);
				for (Index neighbourOfIndex : neighboursOfIndex) {
					Position position = positions[neighbourOfIndex.getRow()][neighbourOfIndex.getColumn()];
					if (position.getPip() == secondPip && !position.isOccupied()) {
						matchingNeighboursOfIndex.add(new PairOfIndices(index, neighbourOfIndex));
					}
				}
			}
			matchingNeighboursOfIndices.put(matchingIndexFirstPip.getKey(), matchingNeighboursOfIndex);
		}
		return matchingNeighboursOfIndices;
	}

	/**
	 * Removes duplicate pairs of indices for each domino.
	 * 
	 * @param pairs
	 *            The pairs of indices
	 * @return The pairs of indices without duplicates
	 */
	private Map<Domino, List<PairOfIndices>> removeDuplicates(final Map<Domino, List<PairOfIndices>> matchingIndices) {
		Map<Domino, List<PairOfIndices>> pairsWithoutDuplicates = new HashMap<>();
		for (Map.Entry<Domino, List<PairOfIndices>> matchingIndex : matchingIndices.entrySet()) {
			List<PairOfIndices> pairsWithDuplicates = new ArrayList<>(matchingIndex.getValue());
			List<PairOfIndices> toRemove = new ArrayList<>();
			for (PairOfIndices pair : pairsWithDuplicates) {
				if (pairsWithDuplicates.contains(pair.switchIndices()) && !toRemove.contains(pair.switchIndices())) {
					toRemove.add(pair);
				}
			}
			for (PairOfIndices remove : toRemove) {
				pairsWithDuplicates.remove(remove);
			}
			pairsWithoutDuplicates.put(matchingIndex.getKey(), pairsWithDuplicates);
		}
		return pairsWithoutDuplicates;
	}

	/**
	 * Finds the list of neighbouring indices for the index
	 * 
	 * @param index
	 *            The index
	 * @return The list of neighbouring indices
	 */
	private List<Index> findNeighboursOfIndex(final Index index) {
		if (index.getRow() == 0 && index.getColumn() == 0) {
			return Arrays.asList(new Index(0, 1), new Index(1, 0));
		} else if (index.getRow() == 0 && index.getColumn() == 7) {
			return Arrays.asList(new Index(0, 6), new Index(1, 7));
		} else if (index.getRow() == 6 && index.getColumn() == 0) {
			return Arrays.asList(new Index(6, 1), new Index(5, 0));
		} else if (index.getRow() == 6 && index.getColumn() == 7) {
			return Arrays.asList(new Index(6, 6), new Index(5, 7));
		} else if (index.getRow() == 0) {
			return Arrays.asList(new Index(0, index.getColumn() - 1), new Index(0, index.getColumn() + 1),
					new Index(1, index.getColumn()));
		} else if (index.getRow() == 6) {
			return Arrays.asList(new Index(6, index.getColumn() - 1), new Index(6, index.getColumn() + 1),
					new Index(5, index.getColumn()));
		} else if (index.getColumn() == 0) {
			return Arrays.asList(new Index(index.getRow() - 1, 0), new Index(index.getRow() + 1, 0),
					new Index(index.getRow(), 1));
		} else if (index.getColumn() == 7) {
			return Arrays.asList(new Index(index.getRow() - 1, 7), new Index(index.getRow() + 1, 7),
					new Index(index.getRow(), 6));
		} else {
			return Arrays.asList(new Index(index.getRow() - 1, index.getColumn()),
					new Index(index.getRow() + 1, index.getColumn()), new Index(index.getRow(), index.getColumn() - 1),
					new Index(index.getRow(), index.getColumn() + 1));
		}
	}

	/**
	 * An index on the board.
	 */
	public static class Index {

		/** The row */
		private int row;

		/** The column */
		private int col;

		/**
		 * Constructor
		 * 
		 * @param row
		 *            The row
		 * @param col
		 *            The column
		 */
		public Index(int row, int col) {
			this.row = row;
			this.col = col;
		}

		/**
		 * Returns the row of the index.
		 * 
		 * @return The row
		 */
		public int getRow() {
			return row;
		}

		/**
		 * Returns the column of the index.
		 * 
		 * @return The column
		 */
		public int getColumn() {
			return col;
		}

		@Override
		public boolean equals(Object index) {
			return row == ((Index) index).getRow() && col == ((Index) index).getColumn();
		}
	}

	/**
	 * A pair of two adjacent indices on the board.
	 */
	public static class PairOfIndices {

		/** The first index */
		private Index first;

		/** The second index */
		private Index second;

		/**
		 * Constructor
		 * 
		 * @param first
		 *            The first index
		 * @param second
		 *            The second index
		 */
		public PairOfIndices(final Index first, final Index second) {
			this.first = first;
			this.second = second;
		}

		/**
		 * Returns the first index.
		 * 
		 * @return The first index
		 */
		public Index getFirstIndex() {
			return first;
		}

		/**
		 * Returns the second index.
		 * 
		 * @return The second index
		 */
		public Index getSecondIndex() {
			return second;
		}

		/**
		 * Returns a pair of indices with the indices switched.
		 * 
		 * @return A pair of indices with the indices switched
		 */
		public PairOfIndices switchIndices() {
			return new PairOfIndices(second, first);
		}

		@Override
		public boolean equals(Object pair) {
			if (first.equals(((PairOfIndices) pair).getFirstIndex())
					&& second.equals(((PairOfIndices) pair).getSecondIndex())) {
				return true;
			}
			return false;
		}
	}
}
