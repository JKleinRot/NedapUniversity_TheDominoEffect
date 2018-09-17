package test.board;

import static org.junit.Assert.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import main.board.Position;

public class PositionTestCase {

	private Position position;
	
	@BeforeEach
	public void setup() {
		position = new Position(5);
	}
	
	@Test
	public void testUnoccupiedPosition() {
		assertEquals(-1, position.getBone());
		assertFalse(position.isOccupied());
		assertEquals(5, position.getPip());
	}
	
	@Test
	public void testOccupiedPosition() {
		Position positionWithBone = position.withBone(21);
		
		assertEquals(21, positionWithBone.getBone());
		assertTrue(positionWithBone.isOccupied());
		assertEquals(5, positionWithBone.getPip());
	}
}
