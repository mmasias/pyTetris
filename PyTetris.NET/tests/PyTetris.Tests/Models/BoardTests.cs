using PyTetris.Models;
using Xunit;

namespace PyTetris.Tests.Models
{
    public class BoardTests
    {
        [Fact]
        public void Constructor_InitializesWithCorrectDimensions()
        {
            // Arrange & Act
            var board = new Board(10, 20);

            // Assert
            Assert.Equal(10, board.Width);
            Assert.Equal(20, board.Height);
        }

        [Fact]
        public void Constructor_InitializesGridWithEmptySpaces()
        {
            // Arrange & Act
            var board = new Board(5, 5);

            // Assert
            var grid = board.GetGrid();
            for (int i = 0; i < 5; i++)
            {
                for (int j = 0; j < 5; j++)
                {
                    Assert.Equal('.', grid[i, j]);
                }
            }
        }

        [Fact]
        public void GetCell_ReturnsCorrectCell()
        {
            // Arrange
            var board = new Board(3, 3);
            var grid = board.GetGrid();
            grid[1, 1] = 'X';

            // Act & Assert
            Assert.Equal('X', board.GetCell(1, 1));
            Assert.Equal('.', board.GetCell(0, 0));
        }

        [Fact]
        public void GetCell_ReturnsSpaceForOutOfBounds()
        {
            // Arrange
            var board = new Board(3, 3);

            // Act & Assert
            Assert.Equal(' ', board.GetCell(-1, 0));
            Assert.Equal(' ', board.GetCell(0, -1));
            Assert.Equal(' ', board.GetCell(3, 0));
            Assert.Equal(' ', board.GetCell(0, 3));
        }

        [Fact]
        public void SetCurrentPiece_SetsPositionToCenter()
        {
            // Arrange
            var board = new Board(10, 20);
            bool[,] shape = { { true, true } }; // width 2
            var piece = new Piece(shape);

            // Act
            board.SetCurrentPiece(piece);

            // Assert
            Assert.Equal(piece, board.CurrentPiece);
            Assert.Equal(4, piece.Position.X); // (10 - 2) / 2 = 4
            Assert.Equal(0, piece.Position.Y);
        }

        [Fact]
        public void CanMovePiece_ReturnsTrueForValidMove()
        {
            // Arrange
            var board = new Board(5, 5);
            bool[,] shape = { { true } };
            var piece = new Piece(shape);
            piece.Position.X = 2;
            piece.Position.Y = 2;

            // Act & Assert
            Assert.True(board.CanMovePiece(piece, 1, 0)); // move right
            Assert.True(board.CanMovePiece(piece, -1, 0)); // move left
            Assert.True(board.CanMovePiece(piece, 0, 1)); // move down
            Assert.True(board.CanMovePiece(piece, 0, 0)); // stay in place
        }

        [Fact]
        public void CanMovePiece_ReturnsFalseForBoundaryCollision()
        {
            // Arrange
            var board = new Board(5, 5);
            bool[,] shape = { { true } };
            var piece = new Piece(shape);

            // Test left boundary
            piece.Position.X = 0;
            piece.Position.Y = 0;
            Assert.False(board.CanMovePiece(piece, -1, 0));

            // Test right boundary
            piece.Position.X = 4;
            piece.Position.Y = 0;
            Assert.False(board.CanMovePiece(piece, 1, 0));

            // Test bottom boundary
            piece.Position.X = 0;
            piece.Position.Y = 4;
            Assert.False(board.CanMovePiece(piece, 0, 1));
        }

        [Fact]
        public void CanMovePiece_ReturnsFalseForOccupiedCell()
        {
            // Arrange
            var board = new Board(5, 5);
            var grid = board.GetGrid();
            grid[2, 2] = 'X'; // occupied cell

            bool[,] shape = { { true } };
            var piece = new Piece(shape);
            piece.Position.X = 1;
            piece.Position.Y = 2;

            // Act & Assert
            Assert.False(board.CanMovePiece(piece, 1, 0)); // would move to occupied cell
        }

        [Fact]
        public void PlacePiece_PlacesSymbolOnGrid()
        {
            // Arrange
            var board = new Board(5, 5);
            bool[,] shape = { 
                { true, false },
                { true, true }
            };
            var piece = new Piece(shape);
            piece.Position.X = 1;
            piece.Position.Y = 1;

            // Act
            board.PlacePiece(piece, 'T');

            // Assert
            var grid = board.GetGrid();
            Assert.Equal('T', grid[1, 1]); // shape[0,0]
            Assert.Equal('.', grid[1, 2]); // shape[0,1] = false
            Assert.Equal('T', grid[2, 1]); // shape[1,0]
            Assert.Equal('T', grid[2, 2]); // shape[1,1]
        }

        [Fact]
        public void PlacePiece_IgnoresOutOfBoundsPositions()
        {
            // Arrange
            var board = new Board(3, 3);
            bool[,] shape = { { true, true } };
            var piece = new Piece(shape);
            piece.Position.X = 2; // second column would be out of bounds
            piece.Position.Y = 0;

            // Act
            board.PlacePiece(piece, 'X');

            // Assert
            var grid = board.GetGrid();
            Assert.Equal('X', grid[0, 2]); // first part placed
            // Second part should be ignored (out of bounds)
        }

        [Fact]
        public void ClearCompleteLines_RemovesFullLines()
        {
            // Arrange
            var board = new Board(3, 3);
            var grid = board.GetGrid();
            
            // Fill bottom line completely
            grid[2, 0] = 'X';
            grid[2, 1] = 'X';
            grid[2, 2] = 'X';
            
            // Partially fill middle line
            grid[1, 0] = 'X';
            grid[1, 1] = '.';
            grid[1, 2] = 'X';

            // Act
            int linesCleared = board.ClearCompleteLines();

            // Assert
            Assert.Equal(1, linesCleared);
            
            // Check that the bottom line now contains what was in the middle line
            Assert.Equal('X', grid[2, 0]);
            Assert.Equal('.', grid[2, 1]);
            Assert.Equal('X', grid[2, 2]);
            
            // Check that middle line now contains what was in the top line (empty)
            Assert.Equal('.', grid[1, 0]);
            Assert.Equal('.', grid[1, 1]);
            Assert.Equal('.', grid[1, 2]);
            
            // Check that top line is now empty
            Assert.Equal('.', grid[0, 0]);
            Assert.Equal('.', grid[0, 1]);
            Assert.Equal('.', grid[0, 2]);
        }

        [Fact]
        public void ClearCompleteLines_ReturnsZeroWhenNoCompleteLines()
        {
            // Arrange
            var board = new Board(3, 3);
            var grid = board.GetGrid();
            
            // Partially fill a line
            grid[2, 0] = 'X';
            grid[2, 1] = '.';
            grid[2, 2] = 'X';

            // Act
            int linesCleared = board.ClearCompleteLines();

            // Assert
            Assert.Equal(0, linesCleared);
        }

        [Fact]
        public void ClearCompleteLines_HandlesMultipleLines()
        {
            // Arrange
            var board = new Board(2, 3);
            var grid = board.GetGrid();
            
            // Fill bottom two lines completely
            grid[1, 0] = 'X';
            grid[1, 1] = 'X';
            grid[2, 0] = 'Y';
            grid[2, 1] = 'Y';

            // Act
            int linesCleared = board.ClearCompleteLines();

            // Assert
            Assert.Equal(2, linesCleared);
            
            // All lines should now be empty
            for (int i = 0; i < 3; i++)
            {
                for (int j = 0; j < 2; j++)
                {
                    Assert.Equal('.', grid[i, j]);
                }
            }
        }

        [Fact]
        public void CanRotatePiece_ReturnsTrueForValidRotation()
        {
            // Arrange
            var board = new Board(5, 5);
            bool[,] shape = { { true, false }, { true, true } };
            var piece = new Piece(shape);
            piece.Position.X = 2;
            piece.Position.Y = 2;

            // Act & Assert
            Assert.True(board.CanRotatePiece(piece, true));
            Assert.True(board.CanRotatePiece(piece, false));
        }

        [Fact]
        public void CanRotatePiece_ReturnsFalseForInvalidRotation()
        {
            // Arrange
            var board = new Board(3, 3);
            bool[,] shape = { { true, true, true } }; // 3 wide piece
            var piece = new Piece(shape);
            piece.Position.X = 1;
            piece.Position.Y = 2;

            // Act & Assert - rotating would make it 3 tall, going out of bounds
            Assert.False(board.CanRotatePiece(piece, true));
        }
    }
}