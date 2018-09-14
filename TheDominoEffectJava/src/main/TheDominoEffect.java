package main;

import java.util.Scanner;

import main.solver.Solver;
import main.solver.SolverImpl;

/**
 * The main class to run to find all possible solutions for a domino grid.
 */
public class TheDominoEffect {

	/** The scanner to read input */
	private static Scanner in;
	
	private final Solver solver;

	/**
	 * Constructor
	 */
	public TheDominoEffect() {
		in = new Scanner(System.in);
		solver = new SolverImpl();
	}

	public void run() {
		String input = readInput("Please enter the 7 x 8 domino grid. Enter each number starting at the top left number and work your way down row wise to the bottom right number of the grid:");
		solver.solve(input);
	}

	/**
	 * Reads the input after displaying the prompt.
	 * 
	 * @param prompt
	 *            The prompt on the console
	 * @return The input
	 */
	private String readInput(final String prompt) {
		String input = null;
		System.out.println(prompt);
		if (in.hasNextLine()) {
			input = in.nextLine();
		}
		return input;
	}

	public static void main(String args[]) {
		TheDominoEffect theDominoEffect = new TheDominoEffect();
		theDominoEffect.run();
	}
}
