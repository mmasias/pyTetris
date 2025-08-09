"""Test Piece class."""

import pytest
from python_src.piece import Piece
from python_src.position import Position


class TestPiece:
    """Test cases for Piece class."""
    
    def test_init(self):
        """Test piece initialization."""
        shape = [[True, False], [True, True]]
        piece = Piece(shape)
        
        assert piece.get_shape() == shape
        assert piece.get_position().x == 0
        assert piece.get_position().y == 0
    
    def test_shape_deep_copy(self):
        """Test that shape is deep copied."""
        original_shape = [[True, False], [True, True]]
        piece = Piece(original_shape)
        
        # Modify original shape
        original_shape[0][0] = False
        
        # Piece shape should be unchanged
        assert piece.get_shape()[0][0] is True
    
    def test_movement(self):
        """Test piece movement."""
        shape = [[True]]
        piece = Piece(shape)
        
        # Test move down
        piece.move_down()
        assert piece.get_position().y == 1
        
        # Test move left
        piece.move_left()
        assert piece.get_position().x == -1
        
        # Test move right
        piece.move_right()
        assert piece.get_position().x == 0
    
    def test_rotate_clockwise(self):
        """Test clockwise rotation."""
        # L-shaped piece
        shape = [
            [True, False],
            [True, True]
        ]
        piece = Piece(shape)
        
        piece.rotate_clockwise()
        expected = [
            [True, True],
            [True, False]
        ]
        assert piece.get_shape() == expected
    
    def test_rotate_counter_clockwise(self):
        """Test counter-clockwise rotation."""
        # Test with a specific shape
        shape = [
            [True, True, False],
            [False, True, False]
        ]
        piece = Piece(shape)
        
        piece.rotate_counter_clockwise()
        expected = [
            [False, False],
            [True, True],
            [True, False]
        ]
        assert piece.get_shape() == expected
    
    def test_rotate_alias(self):
        """Test that rotate() is an alias for rotate_clockwise()."""
        shape = [[True, False], [True, True]]
        piece1 = Piece(shape)
        piece2 = Piece(shape)
        
        piece1.rotate()
        piece2.rotate_clockwise()
        
        assert piece1.get_shape() == piece2.get_shape()