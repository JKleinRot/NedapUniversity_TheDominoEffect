package test.board;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import main.board.Board;
import main.board.Board.Index;
import main.board.Board.PairOfIndices;
import main.board.Dominos;
import main.board.Dominos.Domino;
import main.board.Dominos.Pips;
import main.board.Position;
import main.exception.InvalidInputException;
import main.exception.InvalidInputGridException;
import main.exception.InvalidInputSizeException;

public class BoardTestCase {
	
	private Board board;
	
	private Dominos dominos;
	
	private Domino domino;
	
	@BeforeEach
	public void setup() throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException {
		board = new Board("66265241132010341324665410432112513604555540260360534203");
		dominos = new Dominos();
		domino = dominos.getDomino(new Pips(0,0));
	}
	
	@Test
	public void testConstructor() {
		assertThrows(InvalidInputException.class, () -> new Board("jklfjdsleijfk0123456"));
		assertThrows(InvalidInputException.class, () -> new Board("01234567890123456789012345678901234567890123456789012345"));
		assertThrows(InvalidInputSizeException.class, () -> new Board("0123456"));
		assertThrows(InvalidInputSizeException.class, () -> new Board("012345601234560123456012345601234560123456012345601234560123456"));
		assertThrows(InvalidInputGridException.class, () -> new Board("66666666666666666666666666666666666666666666666666666666"));
	}
	
	@Test
	public void testFindMatchingIndices() {
		Map<Domino, List<PairOfIndices>> expectedMatchingIndices = new HashMap<>();
		expectedMatchingIndices.put(domino, Arrays.asList(new PairOfIndices(new Index(6,6), new Index(5,6))));
		
		Map<Domino, List<PairOfIndices>> matchingIndices = board.findMatchingIndices(dominos);
		
		for (PairOfIndices pair : expectedMatchingIndices.get(domino)) {
			assertTrue(matchingIndices.get(domino).contains(pair));
		}
	}
	
	@Test
	public void testPlaceDominos() {
		Board after = board.placeDomino(domino, new PairOfIndices(new Index(6,6), new Index(5,6)));
		
		assertEquals(new Position(0).withBone(domino.getBone()), after.getPosition(new Index(6,6)));
		assertEquals(new Position(0).withBone(domino.getBone()), after.getPosition(new Index(5,6)));
	}
}
