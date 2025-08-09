"""Test Position class."""

import pytest
from python_src.position import Position


class TestPosition:
    """Test cases for Position class."""
    
    def test_init_default(self):
        """Test default initialization."""
        pos = Position()
        assert pos.x == 0
        assert pos.y == 0
    
    def test_init_with_values(self):
        """Test initialization with values."""
        pos = Position(5, 10)
        assert pos.x == 5
        assert pos.y == 10
    
    def test_add(self):
        """Test adding positions."""
        pos1 = Position(3, 4)
        pos2 = Position(2, 1)
        result = pos1.add(pos2)
        
        assert result.x == 5
        assert result.y == 5
        # Original positions should be unchanged
        assert pos1.x == 3
        assert pos1.y == 4
    
    def test_equality(self):
        """Test position equality."""
        pos1 = Position(5, 10)
        pos2 = Position(5, 10)
        pos3 = Position(3, 10)
        
        assert pos1 == pos2
        assert pos1 != pos3
        assert pos1 != "not a position"
    
    def test_repr(self):
        """Test string representation."""
        pos = Position(7, 3)
        assert repr(pos) == "Position(7, 3)"