"""BoardView class - handles display of the game board."""

from typing import List, Optional
from .board import Board
from .piece_view import PieceView
from .console import Console


class BoardView:
    """Handles the visual display of the Tetris game board."""
    
    def __init__(self, console: Console):
        """Initialize the board view.
        
        Args:
            console: Console instance for output
        """
        self.console = console
    
    def display(self, board: Board, current_piece_view: Optional[PieceView]) -> None:
        """Display the game board with current piece.
        
        Args:
            board: Board to display
            current_piece_view: Current piece view or None
        """
        width = board.get_width()
        height = board.get_height()
        
        # Create a copy of the board grid for display
        display_grid = [row[:] for row in board.get_grid()]
        
        # Render the current piece if it exists
        if current_piece_view and board.get_current_piece():
            current_piece_view.render(board.get_current_piece(), display_grid)
        
        # Clear screen and display the board
        self.console.clear_screen()
        
        # Top border
        border_length = ((width + 2) * 2) - 1
        self.console.writeln("<!=" + "=" * border_length + "!>")
        
        # Board content
        for i in range(height):
            self.console.write("<!  ")
            for j in range(width):
                self.console.write(display_grid[i][j] + " ")
            self.console.writeln(" !>")
        
        # Bottom border
        self.console.writeln("<!=" + "=" * border_length + "!>")
        self.console.writeln("\\/" * (width + 4))