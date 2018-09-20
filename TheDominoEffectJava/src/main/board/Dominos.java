package main.board;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * The dominos that can be placed on the board.
 */
public class Dominos {

	/** The dominos */
	private Map<Pips, Integer> dominos;

	/**
	 * Constructor.
	 */
	public Dominos() {
		dominos.put(new Pips(0, 0), 1);
		dominos.put(new Pips(0, 1), 2);
		dominos.put(new Pips(0, 2), 3);
		dominos.put(new Pips(0, 3), 4);
		dominos.put(new Pips(0, 4), 5);
		dominos.put(new Pips(0, 5), 6);
		dominos.put(new Pips(0, 6), 7);
		dominos.put(new Pips(1, 1), 8);
		dominos.put(new Pips(1, 2), 9);
		dominos.put(new Pips(1, 3), 10);
		dominos.put(new Pips(1, 4), 11);
		dominos.put(new Pips(1, 5), 12);
		dominos.put(new Pips(1, 6), 13);
		dominos.put(new Pips(2, 2), 14);
		dominos.put(new Pips(2, 3), 15);
		dominos.put(new Pips(2, 4), 16);
		dominos.put(new Pips(2, 5), 17);
		dominos.put(new Pips(2, 6), 18);
		dominos.put(new Pips(3, 3), 19);
		dominos.put(new Pips(3, 4), 20);
		dominos.put(new Pips(3, 5), 21);
		dominos.put(new Pips(3, 6), 22);
		dominos.put(new Pips(4, 4), 23);
		dominos.put(new Pips(4, 5), 24);
		dominos.put(new Pips(4, 6), 25);
		dominos.put(new Pips(5, 5), 26);
		dominos.put(new Pips(5, 6), 27);
		dominos.put(new Pips(6, 6), 28);
	}

	public Map<Pips, Integer> getDominos() {
		return dominos;
	}

	/**
	 * The pips of a domino.
	 */
	public class Pips {

		/** The first pip */
		private int first;

		/** The second pip */
		private int second;

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
	}
}
