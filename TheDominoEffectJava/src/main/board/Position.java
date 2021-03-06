package main.board;

/**
 * The position on the board where half of the domino can be placed.
 */
public class Position {

	/** The bone */
	private final int bone;

	/** Whether or not the position is occupied */
	private final Boolean isOccupied;

	/** The pip */
	private final int pip;

	/**
	 * Constructor.
	 * 
	 * @param pip
	 *            The pip
	 */
	public Position(final int pip) {
		this.bone = -1;
		this.isOccupied = false;
		this.pip = pip;
	}

	/**
	 * Constructor.
	 * 
	 * @param bone
	 *            The bone
	 * @param pip
	 *            The pip
	 */
	private Position(final int bone, final int pip) {
		this.bone = bone;
		this.isOccupied = true;
		this.pip = pip;
	}

	/**
	 * Returns the bone of the position if it is occupied or -1 otherwise.
	 * 
	 * @return The bone
	 */
	public int getBone() {
		return bone;
	}

	/**
	 * Returns the pip of the position.
	 * 
	 * @return The pip
	 */
	public int getPip() {
		return pip;
	}
	
	/**
	 * Returns whether or not the position is occupied.
	 * 
	 * @return Whether or not the position is occupied
	 */
	public Boolean isOccupied() {
		return isOccupied;
	}

	/**
	 * Returns a new position with the same pip as this position and with the
	 * provided bone.
	 * 
	 * @param bone
	 *            The bone
	 * @return The new position with pip and bone
	 */
	public Position withBone(final int bone) {
		return new Position(bone, pip);
	}

	@Override
	public boolean equals(Object position) {
		return pip == ((Position) position).getPip() && bone == ((Position) position).getBone()
				&& isOccupied == ((Position) position).isOccupied();
	}
}
