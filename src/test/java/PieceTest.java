import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import static org.junit.jupiter.api.Assertions.*;

public class PieceTest {
    
    private Piece piece;
    private boolean[][] testShape;
    
    @BeforeEach
    public void setUp() {
        testShape = new boolean[][] {
            {true, false},
            {true, true}
        };
        piece = new Piece(testShape);
    }
    
    @Test
    public void testConstructor() {
        assertNotNull(piece.getShape());
        assertNotNull(piece.getPosition());
        assertEquals(0, piece.getPosition().getX());
        assertEquals(0, piece.getPosition().getY());
    }
    
    @Test
    public void testGetShape() {
        boolean[][] shape = piece.getShape();
        assertEquals(2, shape.length);
        assertEquals(2, shape[0].length);
        assertTrue(shape[0][0]);
        assertFalse(shape[0][1]);
        assertTrue(shape[1][0]);
        assertTrue(shape[1][1]);
    }
    
    @Test
    public void testMoveDown() {
        Position initialPosition = piece.getPosition();
        int initialY = initialPosition.getY();
        
        piece.moveDown();
        
        assertEquals(initialY + 1, piece.getPosition().getY());
        assertEquals(initialPosition.getX(), piece.getPosition().getX());
    }
    
    @Test
    public void testMoveLeft() {
        Position initialPosition = piece.getPosition();
        int initialX = initialPosition.getX();
        
        piece.moveLeft();
        
        assertEquals(initialX - 1, piece.getPosition().getX());
        assertEquals(initialPosition.getY(), piece.getPosition().getY());
    }
    
    @Test
    public void testMoveRight() {
        Position initialPosition = piece.getPosition();
        int initialX = initialPosition.getX();
        
        piece.moveRight();
        
        assertEquals(initialX + 1, piece.getPosition().getX());
        assertEquals(initialPosition.getY(), piece.getPosition().getY());
    }
    
    @Test
    public void testRotateMethod() {
        piece.rotate();
        
        // After rotation, shape should be transformed
        boolean[][] rotatedShape = piece.getShape();
        assertNotNull(rotatedShape);
        assertEquals(2, rotatedShape.length);
        assertEquals(2, rotatedShape[0].length);
    }
    
    @Test
    public void testRotateClockwise() {
        // Original shape:
        // [true, false]
        // [true, true]
        
        piece.rotateClockwise();
        boolean[][] rotatedShape = piece.getShape();
        
        // Expected after clockwise rotation:
        // [true, true]
        // [true, false]
        assertTrue(rotatedShape[0][0]);
        assertTrue(rotatedShape[0][1]);
        assertTrue(rotatedShape[1][0]);
        assertFalse(rotatedShape[1][1]);
    }
    
    @Test
    public void testRotateCounterClockwise() {
        // Original shape:
        // [true, false]
        // [true, true]
        
        piece.rotateCounterClockwise();
        boolean[][] rotatedShape = piece.getShape();
        
        // Expected after counter-clockwise rotation:
        // [false, true]
        // [true, true]
        assertFalse(rotatedShape[0][0]);
        assertTrue(rotatedShape[0][1]);
        assertTrue(rotatedShape[1][0]);
        assertTrue(rotatedShape[1][1]);
    }
    
    @Test
    public void testFullRotation() {
        boolean[][] originalShape = piece.getShape();
        
        // Four clockwise rotations should return to original
        piece.rotateClockwise();
        piece.rotateClockwise();
        piece.rotateClockwise();
        piece.rotateClockwise();
        
        boolean[][] finalShape = piece.getShape();
        
        assertEquals(originalShape.length, finalShape.length);
        assertEquals(originalShape[0].length, finalShape[0].length);
        
        for (int i = 0; i < originalShape.length; i++) {
            for (int j = 0; j < originalShape[i].length; j++) {
                assertEquals(originalShape[i][j], finalShape[i][j]);
            }
        }
    }
    
    @Test
    public void testRectangularPieceRotation() {
        // Test with a rectangular piece
        boolean[][] rectShape = new boolean[][] {
            {true, true, true, true}
        };
        Piece rectPiece = new Piece(rectShape);
        
        rectPiece.rotateClockwise();
        boolean[][] rotatedShape = rectPiece.getShape();
        
        assertEquals(4, rotatedShape.length);
        assertEquals(1, rotatedShape[0].length);
        
        for (int i = 0; i < 4; i++) {
            assertTrue(rotatedShape[i][0]);
        }
    }
    
    @Test
    public void testMultipleMovements() {
        piece.moveRight();
        piece.moveDown();
        piece.moveLeft();
        piece.moveLeft();
        
        assertEquals(-1, piece.getPosition().getX());
        assertEquals(1, piece.getPosition().getY());
    }
}