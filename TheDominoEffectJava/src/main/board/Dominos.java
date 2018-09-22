package main.board;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The dominos that can be placed on the board.
 */
public class Dominos {

	/** The dominos */
	private final List<Domino> dominos;

	/**
	 * Constructor.
	 */
	public Dominos() {
		dominos = new ArrayList<>();
		dominos.add(new Domino(new Pips(0, 0), 1));
		dominos.add(new Domino(new Pips(0, 1), 2));
		dominos.add(new Domino(new Pips(0, 2), 3));
		dominos.add(new Domino(new Pips(0, 3), 4));
		dominos.add(new Domino(new Pips(0, 4), 5));
		dominos.add(new Domino(new Pips(0, 5), 6));
		dominos.add(new Domino(new Pips(0, 6), 7));
		dominos.add(new Domino(new Pips(1, 1), 8));
		dominos.add(new Domino(new Pips(1, 2), 9));
		dominos.add(new Domino(new Pips(1, 3), 10));
		dominos.add(new Domino(new Pips(1, 4), 11));
		dominos.add(new Domino(new Pips(1, 5), 12));
		dominos.add(new Domino(new Pips(1, 6), 13));
		dominos.add(new Domino(new Pips(2, 2), 14));
		dominos.add(new Domino(new Pips(2, 3), 15));
		dominos.add(new Domino(new Pips(2, 4), 16));
		dominos.add(new Domino(new Pips(2, 5), 17));
		dominos.add(new Domino(new Pips(2, 6), 18));
		dominos.add(new Domino(new Pips(3, 3), 19));
		dominos.add(new Domino(new Pips(3, 4), 20));
		dominos.add(new Domino(new Pips(3, 5), 21));
		dominos.add(new Domino(new Pips(3, 6), 22));
		dominos.add(new Domino(new Pips(4, 4), 23));
		dominos.add(new Domino(new Pips(4, 5), 24));
		dominos.add(new Domino(new Pips(4, 6), 25));
		dominos.add(new Domino(new Pips(5, 5), 26));
		dominos.add(new Domino(new Pips(5, 6), 27));
		dominos.add(new Domino(new Pips(6, 6), 28));
	}

	/**
	 * Constructor for cloning.
	 * 
	 * @param dominos
	 *            The current dominos
	 */
	private Dominos(final List<Domino> dominos) {
		this.dominos = dominos;
	}

	/**
	 * Returns a cloned list of dominos.
	 * 
	 * @return The cloned list of dominos.
	 */
	public Dominos clone() {
		return new Dominos(new ArrayList<>(dominos));
	}
	
	/**
	 * Returns the domino corresponding to the pips.
	 * 
	 * @param pips
	 *            The pips
	 * @return The domino
	 */
	public Domino getDomino(final Pips pips) {
		return dominos.stream()
				.filter(p -> (p.getPips().getFirstPip() == pips.getFirstPip()
						&& p.getPips().getSecondPip() == pips.getSecondPip())
						|| (p.getPips().getFirstPip() == pips.getSecondPip()
								&& p.getPips().getSecondPip() == pips.getFirstPip()))
				.collect(Collectors.toList()).get(0);
	}
	
	/**
	 * Returns the dominos.
	 * 
	 * @return The dominos
	 */
	public List<Domino> getDominos() {
		return dominos;
	}

	/**
	 * Whether or not the list of dominos is empty.
	 * 
	 * @return Whether or not the list of dominos is empty
	 */
	public boolean isEmpty() {
		return dominos.size() == 0;
	}

	/**
	 * Removes the domino from the list of dominos.
	 * 
	 * @param domino
	 *            The domino to remove
	 * @return The resulting list of dominos
	 */
	public Dominos removeDomino(final Domino domino) {
		dominos.remove(domino);
		return this;
	}

	/**
	 * A domino.
	 */
	public static class Domino {

		/** The bone */
		private final int bone;
		
		/** The pips */
		private final Pips pips;

		/**
		 * Constructor.
		 * 
		 * @param pips
		 *            The pips
		 * @param bone
		 *            The bone
		 */
		public Domino(final Pips pips, final int bone) {
			this.pips = pips;
			this.bone = bone;
		}

		/**
		 * Returns the bone.
		 * 
		 * @return The bone
		 */
		public int getBone() {
			return bone;
		}
		
		/**
		 * Returns the pips.
		 * 
		 * @return The pips
		 */
		public Pips getPips() {
			return pips;
		}

		@Override
		public boolean equals(Object domino) {
			if (bone == ((Domino) domino).getBone() && pips.equals(getPips())) {
				return true;
			}
			return false;
		}
	}

	/**
	 * The pips of a domino.
	 */
	public static class Pips {

		/** The first pip */
		private final int first;

		/** The second pip */
		private final int second;

		/**
		 * Constructor.
		 * 
		 * @param first
		 *            The first pip
		 * @param second
		 *            The second pip
		 */
		public Pips(final int first, final int second) {
			this.first = first;
			this.second = second;
		}

		/**
		 * Returns the first pip.
		 * 
		 * @return The first pip
		 */
		public int getFirstPip() {
			return first;
		}

		/**
		 * Returns the second pip.
		 * 
		 * @return The second pip
		 */
		public int getSecondPip() {
			return second;
		}

		@Override
		public boolean equals(Object pips) {
			if (first == ((Pips) pips).getFirstPip() && second == ((Pips) pips).getSecondPip()) {
				return true;
			} else if (first == ((Pips) pips).getSecondPip() && second == ((Pips) pips).getFirstPip()) {
				return true;
			} else {
				return false;
			}
		}
	}
}
