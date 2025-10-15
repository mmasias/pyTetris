import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import static org.junit.jupiter.api.Assertions.*;

public class PieceViewTest {
    
    private PieceView pieceView;
    private Piece piece;
    
    @BeforeEach
    public void setUp() {
        pieceView = new PieceView('X');
        boolean[][] shape = new boolean[][] {
            {true, false},
            {true, true}
        };
        piece = new Piece(shape);
    }
    
    @Test
    public void testConstructor() {
        assertEquals('X', pieceView.getSymbol());
    }
    
    @Test
    public void testGetSymbol() {
        assertEquals('X', pieceView.getSymbol());
    }
    
    @Test
    public void testSetSymbol() {
        pieceView.setSymbol('O');
        assertEquals('O', pieceView.getSymbol());
    }
    
    @Test
    public void testRenderPieceAtOrigin() {
        char[][] displayGrid = new char[5][5];
        // Initialize grid with dots
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 5; j++) {
                displayGrid[i][j] = '.';
            }
        }
        
        piece.getPosition().setX(0);
        piece.getPosition().setY(0);
        
        pieceView.render(piece, displayGrid);
        
        // Check that piece shape was rendered correctly
        assertEquals('X', displayGrid[0][0]);
        assertEquals('.', displayGrid[0][1]);
        assertEquals('X', displayGrid[1][0]);
        assertEquals('X', displayGrid[1][1]);
        
        // Check that other cells remain unchanged
        assertEquals('.', displayGrid[0][2]);
        assertEquals('.', displayGrid[2][0]);
    }
    
    @Test
    public void testRenderPieceAtOffset() {
        char[][] displayGrid = new char[5][5];
        // Initialize grid with dots
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 5; j++) {
                displayGrid[i][j] = '.';
            }
        }
        
        piece.getPosition().setX(2);
        piece.getPosition().setY(1);
        
        pieceView.render(piece, displayGrid);
        
        // Check that piece shape was rendered at the offset position
        assertEquals('X', displayGrid[1][2]);
        assertEquals('.', displayGrid[1][3]);
        assertEquals('X', displayGrid[2][2]);
        assertEquals('X', displayGrid[2][3]);
        
        // Check that other cells remain unchanged
        assertEquals('.', displayGrid[0][0]);
        assertEquals('.', displayGrid[0][2]);
    }
    
    @Test
    public void testRenderPiecePartiallyOutOfBounds() {
        char[][] displayGrid = new char[3][3];
        // Initialize grid with dots
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                displayGrid[i][j] = '.';
            }
        }
        
        // Position piece so it partially goes out of bounds
        piece.getPosition().setX(2);
        piece.getPosition().setY(2);
        
        pieceView.render(piece, displayGrid);
        
        // Only the part that fits should be rendered
        assertEquals('X', displayGrid[2][2]);
        
        // Other cells should remain unchanged
        assertEquals('.', displayGrid[0][0]);
        assertEquals('.', displayGrid[1][1]);
    }
    
    @Test
    public void testRenderPieceCompletelyOutOfBounds() {
        char[][] displayGrid = new char[3][3];
        // Initialize grid with dots
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                displayGrid[i][j] = '.';
            }
        }
        
        // Position piece completely out of bounds
        piece.getPosition().setX(5);
        piece.getPosition().setY(5);
        
        pieceView.render(piece, displayGrid);
        
        // Grid should remain unchanged
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                assertEquals('.', displayGrid[i][j]);
            }
        }
    }
    
    @Test
    public void testRenderPieceAtNegativePosition() {
        char[][] displayGrid = new char[5][5];
        // Initialize grid with dots
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 5; j++) {
                displayGrid[i][j] = '.';
            }
        }
        
        // Position piece at negative y (partially above visible area)
        piece.getPosition().setX(1);
        piece.getPosition().setY(-1);
        
        pieceView.render(piece, displayGrid);
        
        // Only the visible part should be rendered
        assertEquals('X', displayGrid[0][1]);
        assertEquals('X', displayGrid[0][2]);
        
        // Other cells should remain unchanged
        assertEquals('.', displayGrid[1][0]);
        assertEquals('.', displayGrid[1][1]);
    }
    
    @Test
    public void testRenderWithDifferentSymbols() {
        char[][] displayGrid = new char[3][3];
        // Initialize grid with dots
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                displayGrid[i][j] = '.';
            }
        }
        
        piece.getPosition().setX(0);
        piece.getPosition().setY(0);
        
        // Test with different symbols
        char[] symbols = {'#', '*', '@', '%'};
        
        for (char symbol : symbols) {
            pieceView.setSymbol(symbol);
            pieceView.render(piece, displayGrid);
            
            assertEquals(symbol, displayGrid[0][0]);
            assertEquals(symbol, displayGrid[1][0]);
            assertEquals(symbol, displayGrid[1][1]);
        }
    }
    
    @Test
    public void testRenderEmptyPiece() {
        boolean[][] emptyShape = new boolean[][] {
            {false, false},
            {false, false}
        };
        Piece emptyPiece = new Piece(emptyShape);
        
        char[][] displayGrid = new char[3][3];
        // Initialize grid with dots
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                displayGrid[i][j] = '.';
            }
        }
        
        pieceView.render(emptyPiece, displayGrid);
        
        // Grid should remain unchanged
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                assertEquals('.', displayGrid[i][j]);
            }
        }
    }
}