"""Test Board class."""

import pytest
from python_src.board import Board
from python_src.piece import Piece
from python_src.piece_factory import PieceFactory


class TestBoard:
    """Test cases for Board class."""
    
    def test_init(self):
        """Test board initialization."""
        board = Board(10, 20)
        assert board.get_width() == 10
        assert board.get_height() == 20
        
        # Check board is cleared
        grid = board.get_grid()
        for row in grid:
            for cell in row:
                assert cell == '.'
    
    def test_set_current_piece(self):
        """Test setting current piece."""
        board = Board(10, 20)
        piece = PieceFactory.create_i_piece()
        
        board.set_current_piece(piece)
        assert board.get_current_piece() == piece
        
        # Check piece is positioned at top center
        expected_x = 10 // 2 - 4 // 2  # board_width // 2 - piece_width // 2
        assert piece.get_position().x == expected_x
        assert piece.get_position().y == 0
    
    def test_can_move_piece_valid(self):
        """Test valid piece movement."""
        board = Board(10, 20)
        piece = PieceFactory.create_i_piece()
        board.set_current_piece(piece)
        
        # Should be able to move down
        assert board.can_move_piece(piece, 0, 1) is True
        
        # Should be able to move left (if not at edge)
        if piece.get_position().x > 0:
            assert board.can_move_piece(piece, -1, 0) is True
    
    def test_can_move_piece_boundaries(self):
        """Test piece movement at boundaries."""
        board = Board(4, 4)
        piece = PieceFactory.create_i_piece()  # 4 blocks wide
        board.set_current_piece(piece)
        
        # Should not be able to move left (would go out of bounds)
        assert board.can_move_piece(piece, -1, 0) is False
        
        # Should not be able to move right (would go out of bounds)
        assert board.can_move_piece(piece, 1, 0) is False
    
    def test_can_move_piece_collision(self):
        """Test piece collision detection."""
        board = Board(10, 20)
        
        # Place a block on the board
        board.get_grid()[19][5] = 'X'
        
        # Create piece that would collide
        piece = Piece([[True]])
        piece.get_position().x = 5
        piece.get_position().y = 18
        
        # Should not be able to move down (collision)
        assert board.can_move_piece(piece, 0, 1) is False
    
    def test_place_piece(self):
        """Test placing piece on board."""
        board = Board(10, 20)
        piece = Piece([[True, True]])
        piece.get_position().x = 3
        piece.get_position().y = 18
        
        board.place_piece(piece, 'X')
        
        # Check piece was placed correctly
        assert board.get_grid()[18][3] == 'X'
        assert board.get_grid()[18][4] == 'X'
    
    def test_clear_complete_lines(self):
        """Test clearing complete lines."""
        board = Board(4, 4)
        
        # Fill bottom line completely
        for j in range(4):
            board.get_grid()[3][j] = 'X'
        
        # Partially fill second line
        board.get_grid()[2][0] = 'X'
        board.get_grid()[2][1] = 'X'
        
        lines_cleared = board.clear_complete_lines()
        
        assert lines_cleared == 1
        
        # Check second line moved down to bottom
        assert board.get_grid()[3][0] == 'X'
        assert board.get_grid()[3][1] == 'X'
        assert board.get_grid()[3][2] == '.'
        assert board.get_grid()[3][3] == '.'
        
        # Check rows above are empty
        for i in range(3):
            for j in range(4):
                assert board.get_grid()[i][j] == '.'
    
    def test_can_rotate_piece(self):
        """Test piece rotation."""
        board = Board(10, 20)
        piece = PieceFactory.create_t_piece()
        board.set_current_piece(piece)
        
        # Should be able to rotate
        original_shape = [row[:] for row in piece.get_shape()]
        can_rotate = board.can_rotate_piece(piece, True)
        
        assert can_rotate is True
        assert piece.get_shape() != original_shape
    
    def test_can_rotate_piece_blocked(self):
        """Test rotation when blocked."""
        board = Board(3, 3)  # Small board
        piece = PieceFactory.create_i_piece()  # 4 blocks wide
        board.set_current_piece(piece)
        
        # Piece should not be able to rotate in small space
        original_shape = [row[:] for row in piece.get_shape()]
        can_rotate = board.can_rotate_piece(piece, True)
        
        # Rotation should fail and piece should return to original shape
        assert can_rotate is False
        assert piece.get_shape() == original_shape