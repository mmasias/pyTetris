using PyTetris.Models;
using Xunit;

namespace PyTetris.Tests.Models
{
    public class PositionTests
    {
        [Fact]
        public void Constructor_SetsXAndY()
        {
            // Arrange & Act
            var position = new Position(5, 10);

            // Assert
            Assert.Equal(5, position.X);
            Assert.Equal(10, position.Y);
        }

        [Fact]
        public void Properties_CanBeSetAndGet()
        {
            // Arrange
            var position = new Position(0, 0);

            // Act
            position.X = 7;
            position.Y = 3;

            // Assert
            Assert.Equal(7, position.X);
            Assert.Equal(3, position.Y);
        }

        [Fact]
        public void Add_ReturnsNewPositionWithSum()
        {
            // Arrange
            var position1 = new Position(3, 4);
            var position2 = new Position(2, 6);

            // Act
            var result = position1.Add(position2);

            // Assert
            Assert.Equal(5, result.X);
            Assert.Equal(10, result.Y);
            // Verify original positions are unchanged
            Assert.Equal(3, position1.X);
            Assert.Equal(4, position1.Y);
            Assert.Equal(2, position2.X);
            Assert.Equal(6, position2.Y);
        }

        [Fact]
        public void Equals_ReturnsTrueForSameCoordinates()
        {
            // Arrange
            var position1 = new Position(5, 8);
            var position2 = new Position(5, 8);

            // Act & Assert
            Assert.True(position1.Equals(position2));
            Assert.True(position2.Equals(position1));
        }

        [Fact]
        public void Equals_ReturnsFalseForDifferentCoordinates()
        {
            // Arrange
            var position1 = new Position(5, 8);
            var position2 = new Position(5, 9);
            var position3 = new Position(6, 8);

            // Act & Assert
            Assert.False(position1.Equals(position2));
            Assert.False(position1.Equals(position3));
        }

        [Fact]
        public void Equals_ReturnsFalseForNull()
        {
            // Arrange
            var position = new Position(1, 2);

            // Act & Assert
            Assert.False(position.Equals(null));
        }

        [Fact]
        public void Equals_ReturnsFalseForDifferentType()
        {
            // Arrange
            var position = new Position(1, 2);

            // Act & Assert
            Assert.False(position.Equals("not a position"));
        }

        [Fact]
        public void GetHashCode_SameForEqualPositions()
        {
            // Arrange
            var position1 = new Position(3, 7);
            var position2 = new Position(3, 7);

            // Act & Assert
            Assert.Equal(position1.GetHashCode(), position2.GetHashCode());
        }

        [Fact]
        public void ToString_ReturnsCorrectFormat()
        {
            // Arrange
            var position = new Position(12, 34);

            // Act
            var result = position.ToString();

            // Assert
            Assert.Equal("(12, 34)", result);
        }
    }
}