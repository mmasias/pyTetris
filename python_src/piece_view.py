"""PieceView class - handles the visual representation of pieces."""

from typing import List
from .piece import Piece
from .position import Position


class PieceView:
    """Handles the visual representation of a Tetris piece."""
    
    def __init__(self, symbol: str):
        """Initialize piece view with a symbol.
        
        Args:
            symbol: Character to represent this piece type
        """
        self.symbol = symbol
    
    def get_symbol(self) -> str:
        """Get the symbol representing this piece.
        
        Returns:
            Symbol character
        """
        return self.symbol
    
    def set_symbol(self, symbol: str) -> None:
        """Set the symbol representing this piece.
        
        Args:
            symbol: New symbol character
        """
        self.symbol = symbol
    
    def render(self, piece: Piece, display_grid: List[List[str]]) -> None:
        """Render the piece onto the display grid.
        
        Args:
            piece: The Piece to render
            display_grid: 2D grid to render onto
        """
        shape = piece.get_shape()
        pos = piece.get_position()
        
        for i in range(len(shape)):
            for j in range(len(shape[i])):
                if shape[i][j]:
                    x = pos.x + j
                    y = pos.y + i
                    if (0 <= y < len(display_grid) and 
                        0 <= x < len(display_grid[0])):
                        display_grid[y][x] = self.symbol