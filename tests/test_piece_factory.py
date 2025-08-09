"""Test PieceFactory class."""

import pytest
from python_src.piece_factory import PieceFactory


class TestPieceFactory:
    """Test cases for PieceFactory class."""
    
    def test_create_i_piece(self):
        """Test I-piece creation."""
        piece = PieceFactory.create_i_piece()
        expected_shape = [[True, True, True, True]]
        assert piece.get_shape() == expected_shape
    
    def test_create_o_piece(self):
        """Test O-piece creation."""
        piece = PieceFactory.create_o_piece()
        expected_shape = [
            [True, True],
            [True, True]
        ]
        assert piece.get_shape() == expected_shape
    
    def test_create_t_piece(self):
        """Test T-piece creation."""
        piece = PieceFactory.create_t_piece()
        expected_shape = [
            [False, True, False],
            [True, True, True]
        ]
        assert piece.get_shape() == expected_shape
    
    def test_create_l_piece(self):
        """Test L-piece creation."""
        piece = PieceFactory.create_l_piece()
        expected_shape = [
            [True, False],
            [True, False],
            [True, True]
        ]
        assert piece.get_shape() == expected_shape
    
    def test_create_i_piece_view(self):
        """Test I-piece view creation."""
        view = PieceFactory.create_i_piece_view()
        assert view.get_symbol() == 'I'
    
    def test_create_o_piece_view(self):
        """Test O-piece view creation."""
        view = PieceFactory.create_o_piece_view()
        assert view.get_symbol() == 'O'
    
    def test_create_t_piece_view(self):
        """Test T-piece view creation."""
        view = PieceFactory.create_t_piece_view()
        assert view.get_symbol() == 'T'
    
    def test_create_l_piece_view(self):
        """Test L-piece view creation."""
        view = PieceFactory.create_l_piece_view()
        assert view.get_symbol() == 'L'
    
    def test_pieces_have_correct_positions(self):
        """Test that created pieces start at origin."""
        pieces = [
            PieceFactory.create_i_piece(),
            PieceFactory.create_o_piece(),
            PieceFactory.create_t_piece(),
            PieceFactory.create_l_piece()
        ]
        
        for piece in pieces:
            assert piece.get_position().x == 0
            assert piece.get_position().y == 0