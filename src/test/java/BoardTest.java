import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import static org.junit.jupiter.api.Assertions.*;

public class BoardTest {
    
    private Board board;
    private Piece testPiece;
    
    @BeforeEach
    public void setUp() {
        board = new Board(10, 20);
        boolean[][] shape = new boolean[][] {
            {true, true},
            {true, true}
        };
        testPiece = new Piece(shape);
    }
    
    @Test
    public void testConstructor() {
        assertEquals(10, board.getWidth());
        assertEquals(20, board.getHeight());
        
        char[][] grid = board.getGrid();
        assertNotNull(grid);
        assertEquals(20, grid.length);
        assertEquals(10, grid[0].length);
        
        // All cells should be initialized to '.'
        for (int i = 0; i < 20; i++) {
            for (int j = 0; j < 10; j++) {
                assertEquals('.', grid[i][j]);
            }
        }
    }
    
    @Test
    public void testGetWidth() {
        assertEquals(10, board.getWidth());
    }
    
    @Test
    public void testGetHeight() {
        assertEquals(20, board.getHeight());
    }
    
    @Test
    public void testGetGrid() {
        char[][] grid = board.getGrid();
        assertNotNull(grid);
        assertEquals(20, grid.length);
        assertEquals(10, grid[0].length);
    }
    
    @Test
    public void testSetCurrentPiece() {
        board.setCurrentPiece(testPiece);
        
        assertSame(testPiece, board.getCurrentPiece());
        
        // Piece should be centered horizontally and at top
        Position pos = testPiece.getPosition();
        assertEquals(4, pos.getX()); // (10 - 2) / 2 = 4
        assertEquals(0, pos.getY());
    }
    
    @Test
    public void testSetCurrentPieceWithDifferentSizes() {
        // Test with I piece (4 wide)
        boolean[][] iShape = new boolean[][] {
            {true, true, true, true}
        };
        Piece iPiece = new Piece(iShape);
        
        board.setCurrentPiece(iPiece);
        assertEquals(3, iPiece.getPosition().getX()); // (10 - 4) / 2 = 3
        assertEquals(0, iPiece.getPosition().getY());
        
        // Test with single cell piece
        boolean[][] singleShape = new boolean[][] {
            {true}
        };
        Piece singlePiece = new Piece(singleShape);
        
        board.setCurrentPiece(singlePiece);
        assertEquals(5, singlePiece.getPosition().getX()); // (10 - 1) / 2 = 4, but 10/2 - 1/2 = 5 - 0 = 5
        assertEquals(0, singlePiece.getPosition().getY());
    }
    
    @Test
    public void testCanMovePieceValidMove() {
        board.setCurrentPiece(testPiece);
        
        // Should be able to move down
        assertTrue(board.canMovePiece(testPiece, 0, 1));
        
        // Should be able to move left
        assertTrue(board.canMovePiece(testPiece, -1, 0));
        
        // Should be able to move right
        assertTrue(board.canMovePiece(testPiece, 1, 0));
    }
    
    @Test
    public void testCanMovePieceOutOfBounds() {
        board.setCurrentPiece(testPiece);
        
        // Cannot move left from center to out of bounds
        assertFalse(board.canMovePiece(testPiece, -5, 0));
        
        // Cannot move right to out of bounds
        assertFalse(board.canMovePiece(testPiece, 5, 0));
        
        // Cannot move down to out of bounds
        assertFalse(board.canMovePiece(testPiece, 0, 20));
    }
    
    @Test
    public void testCanMovePieceCollision() {
        board.setCurrentPiece(testPiece);
        
        // Place a piece on the board to create obstacle
        board.placePiece(testPiece, 'X');
        
        // Create new piece to test collision
        boolean[][] newShape = new boolean[][] {
            {true}
        };
        Piece newPiece = new Piece(newShape);
        newPiece.getPosition().setX(4);
        newPiece.getPosition().setY(0);
        
        // Should not be able to move down to occupied cell
        assertFalse(board.canMovePiece(newPiece, 0, 0));
    }
    
    @Test
    public void testCanMovePieceAtEdge() {
        board.setCurrentPiece(testPiece);
        
        // Move piece to left edge
        testPiece.getPosition().setX(0);
        assertFalse(board.canMovePiece(testPiece, -1, 0));
        
        // Move piece to right edge
        testPiece.getPosition().setX(8); // 10 - 2 = 8
        assertFalse(board.canMovePiece(testPiece, 1, 0));
        
        // Move piece to bottom edge
        testPiece.getPosition().setY(18); // 20 - 2 = 18
        assertFalse(board.canMovePiece(testPiece, 0, 1));
    }
    
    @Test
    public void testPlacePiece() {
        board.setCurrentPiece(testPiece);
        
        testPiece.getPosition().setX(2);
        testPiece.getPosition().setY(3);
        
        board.placePiece(testPiece, 'X');
        
        char[][] grid = board.getGrid();
        assertEquals('X', grid[3][2]);
        assertEquals('X', grid[3][3]);
        assertEquals('X', grid[4][2]);
        assertEquals('X', grid[4][3]);
        
        // Other cells should remain unchanged
        assertEquals('.', grid[2][2]);
        assertEquals('.', grid[5][2]);
    }
    
    @Test
    public void testPlacePiecePartiallyOutOfBounds() {
        board.setCurrentPiece(testPiece);
        
        // Position piece so it partially goes out of bounds (above board)
        testPiece.getPosition().setX(2);
        testPiece.getPosition().setY(-1);
        
        board.placePiece(testPiece, 'X');
        
        char[][] grid = board.getGrid();
        // Only the visible part should be placed
        assertEquals('X', grid[0][2]);
        assertEquals('X', grid[0][3]);
        
        // Above board should not affect grid
        assertEquals('.', grid[1][2]);
    }
    
    @Test
    public void testClearCompleteLines() {
        // Fill a complete line
        char[][] grid = board.getGrid();
        for (int j = 0; j < 10; j++) {
            grid[19][j] = 'X'; // Bottom line
        }
        
        int linesCleared = board.clearCompleteLines();
        
        assertEquals(1, linesCleared);
        
        // Bottom line should be empty after clearing
        for (int j = 0; j < 10; j++) {
            assertEquals('.', grid[19][j]);
        }
    }
    
    @Test
    public void testClearMultipleLines() {
        char[][] grid = board.getGrid();
        
        // Fill two complete lines
        for (int j = 0; j < 10; j++) {
            grid[18][j] = 'X';
            grid[19][j] = 'X';
        }
        
        int linesCleared = board.clearCompleteLines();
        
        assertEquals(2, linesCleared);
        
        // Both lines should be empty
        for (int j = 0; j < 10; j++) {
            assertEquals('.', grid[18][j]);
            assertEquals('.', grid[19][j]);
        }
    }
    
    @Test
    public void testClearLinesWithGaps() {
        char[][] grid = board.getGrid();
        
        // Fill line with one gap
        for (int j = 0; j < 9; j++) {
            grid[19][j] = 'X';
        }
        // grid[19][9] remains '.'
        
        int linesCleared = board.clearCompleteLines();
        
        assertEquals(0, linesCleared);
        
        // Line should not be cleared
        for (int j = 0; j < 9; j++) {
            assertEquals('X', grid[19][j]);
        }
        assertEquals('.', grid[19][9]);
    }
    
    @Test
    public void testClearLinesWithAboveContent() {
        char[][] grid = board.getGrid();
        
        // Place content above the line to be cleared
        grid[17][0] = 'Y';
        grid[18][1] = 'Z';
        
        // Fill bottom line completely
        for (int j = 0; j < 10; j++) {
            grid[19][j] = 'X';
        }
        
        int linesCleared = board.clearCompleteLines();
        
        assertEquals(1, linesCleared);
        
        // Content should have moved down
        assertEquals('Y', grid[18][0]);
        assertEquals('Z', grid[19][1]);
        
        // Top line should be empty
        for (int j = 0; j < 10; j++) {
            assertEquals('.', grid[0][j]);
        }
    }
    
    @Test
    public void testCanRotatePiece() {
        board.setCurrentPiece(testPiece);
        
        // Should be able to rotate at center
        assertTrue(board.canRotatePiece(testPiece, true));
        assertTrue(board.canRotatePiece(testPiece, false));
    }
    
    @Test
    public void testCanRotatePieceBlocked() {
        board.setCurrentPiece(testPiece);
        
        // Fill surrounding cells to block rotation
        char[][] grid = board.getGrid();
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 8; j++) {
                grid[i][j] = 'X';
            }
        }
        
        // Should not be able to rotate
        assertFalse(board.canRotatePiece(testPiece, true));
        assertFalse(board.canRotatePiece(testPiece, false));
    }
    
    @Test
    public void testCanRotatePieceRestoresOriginalOnFailure() {
        // Create a piece that will fail rotation when positioned at the edge
        boolean[][] lShape = new boolean[][] {
            {true, false},
            {true, false},
            {true, true}
        };
        Piece lPiece = new Piece(lShape);
        board.setCurrentPiece(lPiece);
        
        // Position at right edge where rotation would definitely fail
        lPiece.getPosition().setX(9);
        lPiece.getPosition().setY(0);
        
        // Store original shape
        boolean[][] originalShape = new boolean[lPiece.getShape().length][];
        for (int i = 0; i < lPiece.getShape().length; i++) {
            originalShape[i] = lPiece.getShape()[i].clone();
        }
        
        // Rotation should fail and restore original shape
        boolean rotationSucceeded = board.canRotatePiece(lPiece, true);
        assertFalse(rotationSucceeded, "Rotation should fail at the right edge");
        
        // Verify shape was restored
        boolean[][] currentShape = lPiece.getShape();
        assertEquals(originalShape.length, currentShape.length);
        for (int i = 0; i < originalShape.length; i++) {
            assertEquals(originalShape[i].length, currentShape[i].length);
            for (int j = 0; j < originalShape[i].length; j++) {
                assertEquals(originalShape[i][j], currentShape[i][j]);
            }
        }
    }
}