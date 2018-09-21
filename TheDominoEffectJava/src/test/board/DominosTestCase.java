package test.board;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import main.board.Dominos;
import main.board.Dominos.Domino;
import main.board.Dominos.Pips;

public class DominosTestCase {

	private Dominos dominos;
	
	@BeforeEach
	public void setup() {
		dominos = new Dominos();
	}
	
	@Test
	public void testGetDominos() {
		Pips pips = new Pips(0,0);
		Domino expectedDomino = new Domino(pips, 1);
		Domino domino = dominos.getDomino(pips);
		
		assertEquals(expectedDomino.getBone(), domino.getBone());
		assertEquals(expectedDomino.getPips(), domino.getPips());
	}
}
