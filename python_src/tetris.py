"""Tetris game main logic."""

import random
from typing import Optional
from .board import Board
from .board_view import BoardView
from .console import Console
from .piece import Piece
from .piece_view import PieceView
from .piece_factory import PieceFactory


class Tetris:
    """Main Tetris game controller."""
    
    def __init__(self, width: int, height: int):
        """Initialize the Tetris game.
        
        Args:
            width: Board width
            height: Board height
        """
        self.console = Console()
        self.board = Board(width, height)
        self.board_view = BoardView(self.console)
        self.game_running = True
        self.score = 0
        self.current_piece_view: Optional[PieceView] = None
    
    def spawn_new_piece(self) -> None:
        """Spawn a new random piece at the top of the board."""
        # Available pieces and their views
        pieces = [
            PieceFactory.create_i_piece(),
            PieceFactory.create_o_piece(),
            PieceFactory.create_t_piece(),
            PieceFactory.create_l_piece()
        ]
        
        piece_views = [
            PieceFactory.create_i_piece_view(),
            PieceFactory.create_o_piece_view(),
            PieceFactory.create_t_piece_view(),
            PieceFactory.create_l_piece_view()
        ]
        
        # Randomly select a piece
        random_index = random.randint(0, len(pieces) - 1)
        self.board.set_current_piece(pieces[random_index])
        self.current_piece_view = piece_views[random_index]
    
    def process_user_input(self, user_input: str) -> None:
        """Process user input commands.
        
        Args:
            user_input: User input string
        """
        if not user_input:
            return
        
        command = user_input[0]
        current_piece = self.board.get_current_piece()
        
        if not current_piece:
            return
        
        if command == '4':  # Move left
            if self.board.can_move_piece(current_piece, -1, 0):
                current_piece.move_left()
        elif command == '6':  # Move right
            if self.board.can_move_piece(current_piece, 1, 0):
                current_piece.move_right()
        elif command == '7':  # Rotate counter-clockwise
            self.board.can_rotate_piece(current_piece, False)
        elif command == '9':  # Rotate clockwise
            self.board.can_rotate_piece(current_piece, True)
    
    def game_loop(self) -> None:
        """Main game loop."""
        self.spawn_new_piece()
        
        while self.game_running:
            # Display the board
            self.board_view.display(self.board, self.current_piece_view)
            
            # Get user input
            prompt = (f"Score: {self.score}\\n\\n"
                     "Comando (4=izq, 6=der, 7=rotar↺, 9=rotar↻): ")
            user_input = self.console.read_string(prompt)
            
            # Process user input
            self.process_user_input(user_input)
            
            current_piece = self.board.get_current_piece()
            if not current_piece:
                continue
            
            # Try to move piece down
            if self.board.can_move_piece(current_piece, 0, 1):
                current_piece.move_down()
            else:
                # Piece can't move down, place it permanently
                self.board.place_piece(current_piece, 
                                     self.current_piece_view.get_symbol())
                
                # Clear complete lines
                lines_cleared = self.board.clear_complete_lines()
                self.score += lines_cleared * 100
                
                # Show line clear message
                if lines_cleared > 0:
                    self.console.writeln(f"¡{lines_cleared} línea(s) eliminada(s)!")
                    self.console.read_string("Presiona Enter para continuar...")
                
                # Spawn new piece
                self.spawn_new_piece()
                
                # Check game over
                new_piece = self.board.get_current_piece()
                if new_piece and not self.board.can_move_piece(new_piece, 0, 0):
                    self.game_running = False
                    self.console.writeln("¡GAME OVER!")
                    self.console.writeln(f"Puntuación final: {self.score}")