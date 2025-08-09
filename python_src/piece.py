"""Piece class - represents a Tetris piece with shape and position."""

from typing import List
from .position import Position


class Piece:
    """Represents a Tetris piece with its shape and position."""
    
    def __init__(self, shape: List[List[bool]]):
        """Initialize a piece with its shape.
        
        Args:
            shape: 2D boolean array representing the piece shape
        """
        self.shape = [row[:] for row in shape]  # Deep copy
        self.position = Position(0, 0)
    
    def get_shape(self) -> List[List[bool]]:
        """Get the current shape of the piece.
        
        Returns:
            2D boolean array representing the piece shape
        """
        return self.shape
    
    def get_position(self) -> Position:
        """Get the current position of the piece.
        
        Returns:
            Position instance
        """
        return self.position
    
    def move_down(self) -> None:
        """Move the piece down by one unit."""
        self.position.y += 1
    
    def move_left(self) -> None:
        """Move the piece left by one unit."""
        self.position.x -= 1
    
    def move_right(self) -> None:
        """Move the piece right by one unit."""
        self.position.x += 1
    
    def rotate(self) -> None:
        """Rotate the piece clockwise (alias for rotate_clockwise)."""
        self.rotate_clockwise()
    
    def rotate_clockwise(self) -> None:
        """Rotate the piece 90 degrees clockwise."""
        rows = len(self.shape)
        cols = len(self.shape[0])
        rotated = [[False] * rows for _ in range(cols)]
        
        for i in range(rows):
            for j in range(cols):
                rotated[j][rows - 1 - i] = self.shape[i][j]
        
        self.shape = rotated
    
    def rotate_counter_clockwise(self) -> None:
        """Rotate the piece 90 degrees counter-clockwise."""
        rows = len(self.shape)
        cols = len(self.shape[0])
        rotated = [[False] * rows for _ in range(cols)]
        
        for i in range(rows):
            for j in range(cols):
                rotated[cols - 1 - j][i] = self.shape[i][j]
        
        self.shape = rotated