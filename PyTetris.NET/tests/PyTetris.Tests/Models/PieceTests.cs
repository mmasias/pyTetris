using PyTetris.Models;
using Xunit;

namespace PyTetris.Tests.Models
{
    public class PieceTests
    {
        [Fact]
        public void Constructor_InitializesWithShape()
        {
            // Arrange
            bool[,] shape = { { true, false }, { false, true } };

            // Act
            var piece = new Piece(shape);

            // Assert
            Assert.Equal(shape, piece.GetShape());
            Assert.Equal(0, piece.Position.X);
            Assert.Equal(0, piece.Position.Y);
        }

        [Fact]
        public void GetWidth_ReturnsCorrectWidth()
        {
            // Arrange
            bool[,] shape = { { true, false, true } };
            var piece = new Piece(shape);

            // Act
            var width = piece.GetWidth();

            // Assert
            Assert.Equal(3, width);
        }

        [Fact]
        public void GetHeight_ReturnsCorrectHeight()
        {
            // Arrange
            bool[,] shape = { { true }, { false }, { true } };
            var piece = new Piece(shape);

            // Act
            var height = piece.GetHeight();

            // Assert
            Assert.Equal(3, height);
        }

        [Fact]
        public void MoveDown_IncreasesYPosition()
        {
            // Arrange
            bool[,] shape = { { true } };
            var piece = new Piece(shape);
            piece.Position.Y = 5;

            // Act
            piece.MoveDown();

            // Assert
            Assert.Equal(6, piece.Position.Y);
        }

        [Fact]
        public void MoveLeft_DecreasesXPosition()
        {
            // Arrange
            bool[,] shape = { { true } };
            var piece = new Piece(shape);
            piece.Position.X = 5;

            // Act
            piece.MoveLeft();

            // Assert
            Assert.Equal(4, piece.Position.X);
        }

        [Fact]
        public void MoveRight_IncreasesXPosition()
        {
            // Arrange
            bool[,] shape = { { true } };
            var piece = new Piece(shape);
            piece.Position.X = 3;

            // Act
            piece.MoveRight();

            // Assert
            Assert.Equal(4, piece.Position.X);
        }

        [Fact]
        public void RotateClockwise_RotatesShapeCorrectly()
        {
            // Arrange
            bool[,] shape = { 
                { true, false },
                { true, true }
            };
            var piece = new Piece(shape);

            // Act
            piece.RotateClockwise();

            // Assert
            var rotated = piece.GetShape();
            Assert.Equal(2, rotated.GetLength(0)); // height
            Assert.Equal(2, rotated.GetLength(1)); // width
            
            // The clockwise rotation should produce:
            // Original:    After CW rotation:
            // X .          X X
            // X X          X .
            Assert.True(rotated[0, 0]);   // top-left
            Assert.True(rotated[0, 1]);   // top-right
            Assert.True(rotated[1, 0]);   // bottom-left
            Assert.False(rotated[1, 1]);  // bottom-right
        }

        [Fact]
        public void RotateCounterClockwise_RotatesShapeCorrectly()
        {
            // Arrange
            bool[,] shape = { 
                { true, false },
                { true, true }
            };
            var piece = new Piece(shape);

            // Act
            piece.RotateCounterClockwise();

            // Assert
            var rotated = piece.GetShape();
            Assert.Equal(2, rotated.GetLength(0)); // height
            Assert.Equal(2, rotated.GetLength(1)); // width
            
            // The counter-clockwise rotation should produce:
            // Original:    After CCW rotation:
            // X .          . X
            // X X          X X
            Assert.False(rotated[0, 0]);  // top-left
            Assert.True(rotated[0, 1]);   // top-right
            Assert.True(rotated[1, 0]);   // bottom-left
            Assert.True(rotated[1, 1]);   // bottom-right
        }

        [Fact]
        public void Rotate_CallsRotateClockwise()
        {
            // Arrange
            bool[,] shape = { 
                { true, false },
                { false, true }
            };
            var piece = new Piece(shape);

            // Act
            piece.Rotate();

            // Assert
            var rotated = piece.GetShape();
            Assert.Equal(2, rotated.GetLength(0));
            Assert.Equal(2, rotated.GetLength(1));
            Assert.False(rotated[0, 0]);
            Assert.True(rotated[0, 1]);
            Assert.True(rotated[1, 0]);
            Assert.False(rotated[1, 1]);
        }

        [Fact]
        public void Clone_CreatesIndependentCopy()
        {
            // Arrange
            bool[,] shape = { { true, false }, { false, true } };
            var original = new Piece(shape);
            original.Position.X = 3;
            original.Position.Y = 5;

            // Act
            var clone = original.Clone();

            // Assert
            Assert.Equal(original.Position.X, clone.Position.X);
            Assert.Equal(original.Position.Y, clone.Position.Y);
            Assert.Equal(original.GetWidth(), clone.GetWidth());
            Assert.Equal(original.GetHeight(), clone.GetHeight());

            // Verify they are independent
            clone.Position.X = 10;
            clone.MoveDown();
            Assert.Equal(3, original.Position.X);
            Assert.Equal(5, original.Position.Y);
            Assert.Equal(10, clone.Position.X);
            Assert.Equal(6, clone.Position.Y);
        }

        [Fact]
        public void MultipleRotations_ReturnToOriginalShape()
        {
            // Arrange
            bool[,] originalShape = { 
                { true, false, false },
                { true, true, true }
            };
            var piece = new Piece(originalShape);

            // Act - 4 clockwise rotations should return to original
            piece.RotateClockwise();
            piece.RotateClockwise();
            piece.RotateClockwise();
            piece.RotateClockwise();

            // Assert
            var shape = piece.GetShape();
            Assert.Equal(originalShape.GetLength(0), shape.GetLength(0));
            Assert.Equal(originalShape.GetLength(1), shape.GetLength(1));
            for (int i = 0; i < shape.GetLength(0); i++)
            {
                for (int j = 0; j < shape.GetLength(1); j++)
                {
                    Assert.Equal(originalShape[i, j], shape[i, j]);
                }
            }
        }
    }
}