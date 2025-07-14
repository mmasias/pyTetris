import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class PieceFactoryTest {
    
    @Test
    public void testCreateIPiece() {
        Piece iPiece = PieceFactory.createIPiece();
        assertNotNull(iPiece);
        
        boolean[][] shape = iPiece.getShape();
        assertNotNull(shape);
        assertEquals(1, shape.length);
        assertEquals(4, shape[0].length);
        
        // All elements should be true for I piece
        for (int j = 0; j < shape[0].length; j++) {
            assertTrue(shape[0][j]);
        }
    }
    
    @Test
    public void testCreateOPiece() {
        Piece oPiece = PieceFactory.createOPiece();
        assertNotNull(oPiece);
        
        boolean[][] shape = oPiece.getShape();
        assertNotNull(shape);
        assertEquals(2, shape.length);
        assertEquals(2, shape[0].length);
        
        // All elements should be true for O piece
        for (int i = 0; i < shape.length; i++) {
            for (int j = 0; j < shape[i].length; j++) {
                assertTrue(shape[i][j]);
            }
        }
    }
    
    @Test
    public void testCreateTPiece() {
        Piece tPiece = PieceFactory.createTPiece();
        assertNotNull(tPiece);
        
        boolean[][] shape = tPiece.getShape();
        assertNotNull(shape);
        assertEquals(2, shape.length);
        assertEquals(3, shape[0].length);
        
        // First row: [false, true, false]
        assertFalse(shape[0][0]);
        assertTrue(shape[0][1]);
        assertFalse(shape[0][2]);
        
        // Second row: [true, true, true]
        assertTrue(shape[1][0]);
        assertTrue(shape[1][1]);
        assertTrue(shape[1][2]);
    }
    
    @Test
    public void testCreateLPiece() {
        Piece lPiece = PieceFactory.createLPiece();
        assertNotNull(lPiece);
        
        boolean[][] shape = lPiece.getShape();
        assertNotNull(shape);
        assertEquals(3, shape.length);
        assertEquals(2, shape[0].length);
        
        // First row: [true, false]
        assertTrue(shape[0][0]);
        assertFalse(shape[0][1]);
        
        // Second row: [true, false]
        assertTrue(shape[1][0]);
        assertFalse(shape[1][1]);
        
        // Third row: [true, true]
        assertTrue(shape[2][0]);
        assertTrue(shape[2][1]);
    }
    
    @Test
    public void testCreateIPieceView() {
        PieceView iPieceView = PieceFactory.createIPieceView();
        assertNotNull(iPieceView);
        assertEquals('I', iPieceView.getSymbol());
    }
    
    @Test
    public void testCreateOPieceView() {
        PieceView oPieceView = PieceFactory.createOPieceView();
        assertNotNull(oPieceView);
        assertEquals('O', oPieceView.getSymbol());
    }
    
    @Test
    public void testCreateTPieceView() {
        PieceView tPieceView = PieceFactory.createTPieceView();
        assertNotNull(tPieceView);
        assertEquals('T', tPieceView.getSymbol());
    }
    
    @Test
    public void testCreateLPieceView() {
        PieceView lPieceView = PieceFactory.createLPieceView();
        assertNotNull(lPieceView);
        assertEquals('L', lPieceView.getSymbol());
    }
    
    @Test
    public void testAllPiecesHaveValidShapes() {
        Piece[] pieces = {
            PieceFactory.createIPiece(),
            PieceFactory.createOPiece(),
            PieceFactory.createTPiece(),
            PieceFactory.createLPiece()
        };
        
        for (Piece piece : pieces) {
            assertNotNull(piece);
            boolean[][] shape = piece.getShape();
            assertNotNull(shape);
            assertTrue(shape.length > 0);
            assertTrue(shape[0].length > 0);
            
            // Each piece should have at least one true element
            boolean hasTrueElement = false;
            for (int i = 0; i < shape.length; i++) {
                for (int j = 0; j < shape[i].length; j++) {
                    if (shape[i][j]) {
                        hasTrueElement = true;
                        break;
                    }
                }
                if (hasTrueElement) break;
            }
            assertTrue(hasTrueElement);
        }
    }
    
    @Test
    public void testAllPieceViewsHaveUniqueSymbols() {
        PieceView[] views = {
            PieceFactory.createIPieceView(),
            PieceFactory.createOPieceView(),
            PieceFactory.createTPieceView(),
            PieceFactory.createLPieceView()
        };
        
        char[] symbols = {'I', 'O', 'T', 'L'};
        
        for (int i = 0; i < views.length; i++) {
            assertNotNull(views[i]);
            assertEquals(symbols[i], views[i].getSymbol());
        }
    }
    
    @Test
    public void testPieceInitialPosition() {
        Piece piece = PieceFactory.createIPiece();
        Position pos = piece.getPosition();
        assertNotNull(pos);
        assertEquals(0, pos.getX());
        assertEquals(0, pos.getY());
    }
}