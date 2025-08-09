"""Integration tests for the complete Tetris game."""

import pytest
from python_src.tetris import Tetris
from python_src.piece_factory import PieceFactory


class TestTetrisIntegration:
    """Integration tests for the complete game."""
    
    def test_game_initialization(self):
        """Test that game initializes correctly."""
        game = Tetris(10, 20)
        
        assert game.board.get_width() == 10
        assert game.board.get_height() == 20
        assert game.score == 0
        assert game.game_running is True
    
    def test_spawn_new_piece(self):
        """Test piece spawning."""
        game = Tetris(10, 20)
        game.spawn_new_piece()
        
        assert game.board.get_current_piece() is not None
        assert game.current_piece_view is not None
        
        # Piece should be positioned at top center
        piece = game.board.get_current_piece()
        assert piece.get_position().y == 0
    
    def test_user_input_processing(self):
        """Test user input processing."""
        game = Tetris(10, 20)
        game.spawn_new_piece()
        
        piece = game.board.get_current_piece()
        original_x = piece.get_position().x
        
        # Test left movement
        game.process_user_input("4")
        assert piece.get_position().x == original_x - 1
        
        # Test right movement
        game.process_user_input("6")
        assert piece.get_position().x == original_x
    
    def test_piece_falling_and_placement(self):
        """Test piece falling and placement."""
        game = Tetris(4, 4)  # Small board for testing
        
        # Create a simple piece
        game.board.set_current_piece(PieceFactory.create_o_piece())
        game.current_piece_view = PieceFactory.create_o_piece_view()
        
        piece = game.board.get_current_piece()
        
        # Move piece down until it can't move anymore
        while game.board.can_move_piece(piece, 0, 1):
            piece.move_down()
        
        # Piece should be at bottom
        assert piece.get_position().y == 2  # Height 4 - piece height 2 = 2
        
        # Place the piece
        game.board.place_piece(piece, game.current_piece_view.get_symbol())
        
        # Check that piece was placed on board
        grid = game.board.get_grid()
        assert grid[2][1] == 'O'  # O-piece placed
        assert grid[2][2] == 'O'
        assert grid[3][1] == 'O'
        assert grid[3][2] == 'O'
    
    def test_line_clearing_integration(self):
        """Test line clearing in game context."""
        game = Tetris(4, 4)
        
        # Manually fill bottom line
        grid = game.board.get_grid()
        for j in range(4):
            grid[3][j] = 'X'
        
        # Clear lines and check score
        lines_cleared = game.board.clear_complete_lines()
        game.score += lines_cleared * 100
        
        assert lines_cleared == 1
        assert game.score == 100