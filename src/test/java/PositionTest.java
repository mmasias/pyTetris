import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import static org.junit.jupiter.api.Assertions.*;

public class PositionTest {
    
    private Position position;
    
    @BeforeEach
    public void setUp() {
        position = new Position(5, 3);
    }
    
    @Test
    public void testConstructor() {
        assertEquals(5, position.getX());
        assertEquals(3, position.getY());
    }
    
    @Test
    public void testSetX() {
        position.setX(10);
        assertEquals(10, position.getX());
    }
    
    @Test
    public void testSetY() {
        position.setY(7);
        assertEquals(7, position.getY());
    }
    
    @Test
    public void testAddMethod() {
        Position other = new Position(2, 4);
        Position result = position.add(other);
        
        assertEquals(7, result.getX());
        assertEquals(7, result.getY());
        
        // Verify original positions unchanged
        assertEquals(5, position.getX());
        assertEquals(3, position.getY());
        assertEquals(2, other.getX());
        assertEquals(4, other.getY());
    }
    
    @Test
    public void testAddWithNegativeNumbers() {
        Position other = new Position(-2, -1);
        Position result = position.add(other);
        
        assertEquals(3, result.getX());
        assertEquals(2, result.getY());
    }
    
    @Test
    public void testAddWithZero() {
        Position other = new Position(0, 0);
        Position result = position.add(other);
        
        assertEquals(5, result.getX());
        assertEquals(3, result.getY());
    }
    
    @Test
    public void testCreatePositionWithZero() {
        Position zeroPosition = new Position(0, 0);
        assertEquals(0, zeroPosition.getX());
        assertEquals(0, zeroPosition.getY());
    }
    
    @Test
    public void testCreatePositionWithNegativeValues() {
        Position negativePosition = new Position(-5, -10);
        assertEquals(-5, negativePosition.getX());
        assertEquals(-10, negativePosition.getY());
    }
}