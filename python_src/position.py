"""Position class - represents a 2D coordinate point."""


class Position:
    """Represents a position with x and y coordinates."""
    
    def __init__(self, x: int = 0, y: int = 0):
        """Initialize position with x and y coordinates.
        
        Args:
            x: X coordinate (default: 0)
            y: Y coordinate (default: 0)
        """
        self.x = x
        self.y = y
    
    def add(self, other: 'Position') -> 'Position':
        """Add another position to this one.
        
        Args:
            other: Another Position instance
            
        Returns:
            New Position with added coordinates
        """
        return Position(self.x + other.x, self.y + other.y)
    
    def __repr__(self) -> str:
        """String representation of the position."""
        return f"Position({self.x}, {self.y})"
    
    def __eq__(self, other) -> bool:
        """Check equality with another position."""
        if not isinstance(other, Position):
            return False
        return self.x == other.x and self.y == other.y