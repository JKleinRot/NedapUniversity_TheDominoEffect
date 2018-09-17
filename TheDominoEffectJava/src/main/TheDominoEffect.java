package main;

import java.util.Scanner;

import main.exception.InvalidInputException;
import main.exception.InvalidInputGridException;
import main.exception.InvalidInputSizeException;
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
		try {
			solver.solve(input);
		} catch (InvalidInputException e) {
			System.out.println("The entered domino grid was not valid since it contained characters other than 0..6.");
			run();
		} catch (InvalidInputSizeException e) {
			System.out.println("The entered domino grid was not valid since the input did not contain 56 characters");
			run();
		} catch (InvalidInputGridException e) {
			System.out.println("The entered domino grid was not valid since the amount of each pip in the grid was not correct");
			run();
		}
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
