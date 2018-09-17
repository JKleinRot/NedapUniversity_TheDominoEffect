package test.board;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import main.board.Board;
import main.exception.InvalidInputException;
import main.exception.InvalidInputGridException;
import main.exception.InvalidInputSizeException;

public class BoardTestCase {
	
	private Board board;
	
	@BeforeEach
	public void setup() throws InvalidInputException, InvalidInputSizeException, InvalidInputGridException {
		board = new Board("66265241132010341324665410432112513604555540260360534203");
	}
	
	@Test
	public void testConstructor() {
		assertThrows(InvalidInputException.class, () -> new Board("jklfjdsleijfk0123456"));
		assertThrows(InvalidInputException.class, () -> new Board("01234567890123456789012345678901234567890123456789012345"));
		assertThrows(InvalidInputSizeException.class, () -> new Board("0123456"));
		assertThrows(InvalidInputSizeException.class, () -> new Board("012345601234560123456012345601234560123456012345601234560123456"));
		assertThrows(InvalidInputGridException.class, () -> new Board("66666666666666666666666666666666666666666666666666666666"));
	}
}
