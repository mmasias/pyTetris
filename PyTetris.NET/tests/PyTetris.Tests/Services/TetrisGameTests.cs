using PyTetris.Services;
using Xunit;

namespace PyTetris.Tests.Services
{
    public class TetrisGameTests
    {
        [Fact]
        public void Constructor_InitializesGame()
        {
            // Act
            var game = new TetrisGame(10, 20);

            // Assert
            Assert.NotNull(game.GetBoard());
            Assert.Equal(10, game.GetBoard().Width);
            Assert.Equal(20, game.GetBoard().Height);
            Assert.Equal(0, game.GetScore());
            Assert.True(game.IsGameRunning());
        }

        [Fact]
        public void SpawnNewPiece_SetsPieceOnBoard()
        {
            // Arrange
            var game = new TetrisGame(10, 20);

            // Act
            game.SpawnNewPiece();

            // Assert
            Assert.NotNull(game.GetBoard().CurrentPiece);
            var currentPiece = game.GetBoard().CurrentPiece;
            Assert.NotNull(currentPiece);
            Assert.Equal(0, currentPiece.Position.Y);
            // X position should be centered
            var expectedX = 10 / 2 - currentPiece.GetWidth() / 2;
            Assert.Equal(expectedX, currentPiece.Position.X);
        }

        [Fact]
        public void SpawnNewPiece_GeneratesValidPieceTypes()
        {
            // Arrange
            var game = new TetrisGame(10, 20);
            var pieceTypes = new HashSet<string>();

            // Act - Spawn many pieces to test randomization
            for (int i = 0; i < 50; i++)
            {
                game.SpawnNewPiece();
                var piece = game.GetBoard().CurrentPiece;
                Assert.NotNull(piece);
                var shape = piece.GetShape();
                var shapeSignature = $"{shape.GetLength(0)}x{shape.GetLength(1)}";
                pieceTypes.Add(shapeSignature);
            }

            // Assert - Should have generated different piece types
            Assert.True(pieceTypes.Count > 1);
        }

        [Fact]
        public void GetBoard_ReturnsCorrectBoard()
        {
            // Arrange
            var game = new TetrisGame(5, 8);

            // Act
            var board = game.GetBoard();

            // Assert
            Assert.NotNull(board);
            Assert.Equal(5, board.Width);
            Assert.Equal(8, board.Height);
        }

        [Fact]
        public void GetScore_InitiallyZero()
        {
            // Arrange
            var game = new TetrisGame(10, 20);

            // Act
            var score = game.GetScore();

            // Assert
            Assert.Equal(0, score);
        }

        [Fact]
        public void IsGameRunning_InitiallyTrue()
        {
            // Arrange
            var game = new TetrisGame(10, 20);

            // Act
            var isRunning = game.IsGameRunning();

            // Assert
            Assert.True(isRunning);
        }
    }
}