"""Board class - manages the Tetris game board state."""

from typing import List, Optional
from .piece import Piece
from .position import Position


class Board:
    """Manages the Tetris game board state and piece interactions."""
    
    def __init__(self, width: int, height: int):
        """Initialize the game board.
        
        Args:
            width: Board width in cells
            height: Board height in cells
        """
        self.width = width
        self.height = height
        self.grid: List[List[str]] = []
        self.current_piece: Optional[Piece] = None
        self._clear_board()
    
    def _clear_board(self) -> None:
        """Clear the board by filling with empty cells."""
        self.grid = [['.' for _ in range(self.width)] 
                    for _ in range(self.height)]
    
    def get_width(self) -> int:
        """Get board width.
        
        Returns:
            Board width
        """
        return self.width
    
    def get_height(self) -> int:
        """Get board height.
        
        Returns:
            Board height
        """
        return self.height
    
    def get_grid(self) -> List[List[str]]:
        """Get the board grid.
        
        Returns:
            2D grid representing the board
        """
        return self.grid
    
    def set_current_piece(self, piece: Piece) -> None:
        """Set the current active piece.
        
        Args:
            piece: Piece to set as current
        """
        self.current_piece = piece
        # Position piece at the top center of the board
        piece_width = len(piece.get_shape()[0])
        piece.get_position().x = self.width // 2 - piece_width // 2
        piece.get_position().y = 0
    
    def get_current_piece(self) -> Optional[Piece]:
        """Get the current active piece.
        
        Returns:
            Current piece or None
        """
        return self.current_piece
    
    def can_move_piece(self, piece: Piece, delta_x: int, delta_y: int) -> bool:
        """Check if a piece can move to a new position.
        
        Args:
            piece: Piece to check
            delta_x: X movement delta
            delta_y: Y movement delta
            
        Returns:
            True if move is valid
        """
        shape = piece.get_shape()
        pos = piece.get_position()
        
        for i in range(len(shape)):
            for j in range(len(shape[i])):
                if shape[i][j]:
                    new_x = pos.x + j + delta_x
                    new_y = pos.y + i + delta_y
                    
                    # Check boundaries
                    if new_x < 0 or new_x >= self.width or new_y >= self.height:
                        return False
                    
                    # Check collision with existing pieces
                    if new_y >= 0 and self.grid[new_y][new_x] != '.':
                        return False
        
        return True
    
    def can_rotate_piece(self, piece: Piece, clockwise: bool) -> bool:
        """Check if a piece can rotate and perform the rotation if possible.
        
        Args:
            piece: Piece to rotate
            clockwise: True for clockwise, False for counter-clockwise
            
        Returns:
            True if rotation was successful
        """
        # Perform rotation
        if clockwise:
            piece.rotate_clockwise()
        else:
            piece.rotate_counter_clockwise()
        
        # Check if rotation is valid
        can_rotate = self.can_move_piece(piece, 0, 0)
        
        # If invalid, reverse the rotation
        if not can_rotate:
            if clockwise:
                # Reverse clockwise rotation with 3 counter-clockwise rotations
                piece.rotate_counter_clockwise()
                piece.rotate_counter_clockwise()
                piece.rotate_counter_clockwise()
            else:
                # Reverse counter-clockwise rotation with 3 clockwise rotations
                piece.rotate_clockwise()
                piece.rotate_clockwise()
                piece.rotate_clockwise()
        
        return can_rotate
    
    def place_piece(self, piece: Piece, symbol: str) -> None:
        """Place a piece permanently on the board.
        
        Args:
            piece: Piece to place
            symbol: Symbol to use for the piece
        """
        shape = piece.get_shape()
        pos = piece.get_position()
        
        for i in range(len(shape)):
            for j in range(len(shape[i])):
                if shape[i][j]:
                    x = pos.x + j
                    y = pos.y + i
                    if (0 <= y < self.height and 0 <= x < self.width):
                        self.grid[y][x] = symbol
    
    def clear_complete_lines(self) -> int:
        """Clear complete horizontal lines and return count.
        
        Returns:
            Number of lines cleared
        """
        lines_cleared = 0
        
        # Check from bottom to top
        i = self.height - 1
        while i >= 0:
            # Check if line is complete
            is_complete = all(self.grid[i][j] != '.' 
                            for j in range(self.width))
            
            if is_complete:
                # Remove the complete line
                del self.grid[i]
                # Add new empty line at the top
                self.grid.insert(0, ['.' for _ in range(self.width)])
                lines_cleared += 1
                # Don't decrement i since we removed a line
            else:
                i -= 1
        
        return lines_cleared