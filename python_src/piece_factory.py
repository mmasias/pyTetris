"""PieceFactory class - creates different types of Tetris pieces."""

from .piece import Piece
from .piece_view import PieceView


class PieceFactory:
    """Factory class for creating Tetris pieces and their views."""
    
    @staticmethod
    def create_i_piece() -> Piece:
        """Create an I-shaped piece (straight line).
        
        Returns:
            I-piece instance
        """
        shape = [
            [True, True, True, True]
        ]
        return Piece(shape)
    
    @staticmethod
    def create_o_piece() -> Piece:
        """Create an O-shaped piece (square).
        
        Returns:
            O-piece instance
        """
        shape = [
            [True, True],
            [True, True]
        ]
        return Piece(shape)
    
    @staticmethod
    def create_t_piece() -> Piece:
        """Create a T-shaped piece.
        
        Returns:
            T-piece instance
        """
        shape = [
            [False, True, False],
            [True, True, True]
        ]
        return Piece(shape)
    
    @staticmethod
    def create_l_piece() -> Piece:
        """Create an L-shaped piece.
        
        Returns:
            L-piece instance
        """
        shape = [
            [True, False],
            [True, False],
            [True, True]
        ]
        return Piece(shape)
    
    @staticmethod
    def create_i_piece_view() -> PieceView:
        """Create a view for I-piece.
        
        Returns:
            PieceView for I-piece
        """
        return PieceView('I')
    
    @staticmethod
    def create_o_piece_view() -> PieceView:
        """Create a view for O-piece.
        
        Returns:
            PieceView for O-piece
        """
        return PieceView('O')
    
    @staticmethod
    def create_t_piece_view() -> PieceView:
        """Create a view for T-piece.
        
        Returns:
            PieceView for T-piece
        """
        return PieceView('T')
    
    @staticmethod
    def create_l_piece_view() -> PieceView:
        """Create a view for L-piece.
        
        Returns:
            PieceView for L-piece
        """
        return PieceView('L')