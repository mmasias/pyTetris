using PyTetris.Factories;
using Xunit;

namespace PyTetris.Tests.Factories
{
    public class PieceFactoryTests
    {
        [Fact]
        public void CreateIPiece_ReturnsCorrectShape()
        {
            // Act
            var piece = PieceFactory.CreateIPiece();

            // Assert
            var shape = piece.GetShape();
            Assert.Equal(1, shape.GetLength(0)); // height
            Assert.Equal(4, shape.GetLength(1)); // width
            
            // Verify the shape pattern
            Assert.True(shape[0, 0]);
            Assert.True(shape[0, 1]);
            Assert.True(shape[0, 2]);
            Assert.True(shape[0, 3]);
        }

        [Fact]
        public void CreateOPiece_ReturnsCorrectShape()
        {
            // Act
            var piece = PieceFactory.CreateOPiece();

            // Assert
            var shape = piece.GetShape();
            Assert.Equal(2, shape.GetLength(0)); // height
            Assert.Equal(2, shape.GetLength(1)); // width
            
            // Verify the shape pattern
            Assert.True(shape[0, 0]);
            Assert.True(shape[0, 1]);
            Assert.True(shape[1, 0]);
            Assert.True(shape[1, 1]);
        }

        [Fact]
        public void CreateTPiece_ReturnsCorrectShape()
        {
            // Act
            var piece = PieceFactory.CreateTPiece();

            // Assert
            var shape = piece.GetShape();
            Assert.Equal(2, shape.GetLength(0)); // height
            Assert.Equal(3, shape.GetLength(1)); // width
            
            // Verify the shape pattern
            Assert.False(shape[0, 0]);
            Assert.True(shape[0, 1]);
            Assert.False(shape[0, 2]);
            Assert.True(shape[1, 0]);
            Assert.True(shape[1, 1]);
            Assert.True(shape[1, 2]);
        }

        [Fact]
        public void CreateLPiece_ReturnsCorrectShape()
        {
            // Act
            var piece = PieceFactory.CreateLPiece();

            // Assert
            var shape = piece.GetShape();
            Assert.Equal(3, shape.GetLength(0)); // height
            Assert.Equal(2, shape.GetLength(1)); // width
            
            // Verify the shape pattern
            Assert.True(shape[0, 0]);
            Assert.False(shape[0, 1]);
            Assert.True(shape[1, 0]);
            Assert.False(shape[1, 1]);
            Assert.True(shape[2, 0]);
            Assert.True(shape[2, 1]);
        }

        [Fact]
        public void CreateIPieceView_ReturnsCorrectSymbol()
        {
            // Act
            var pieceView = PieceFactory.CreateIPieceView();

            // Assert
            Assert.Equal('I', pieceView.Symbol);
        }

        [Fact]
        public void CreateOPieceView_ReturnsCorrectSymbol()
        {
            // Act
            var pieceView = PieceFactory.CreateOPieceView();

            // Assert
            Assert.Equal('O', pieceView.Symbol);
        }

        [Fact]
        public void CreateTPieceView_ReturnsCorrectSymbol()
        {
            // Act
            var pieceView = PieceFactory.CreateTPieceView();

            // Assert
            Assert.Equal('T', pieceView.Symbol);
        }

        [Fact]
        public void CreateLPieceView_ReturnsCorrectSymbol()
        {
            // Act
            var pieceView = PieceFactory.CreateLPieceView();

            // Assert
            Assert.Equal('L', pieceView.Symbol);
        }

        [Fact]
        public void CreateRandomPiece_ReturnsValidPieceAndView()
        {
            // Act
            var (piece, view) = PieceFactory.CreateRandomPiece();

            // Assert
            Assert.NotNull(piece);
            Assert.NotNull(view);
            
            // Verify the piece has a valid shape
            var shape = piece.GetShape();
            Assert.True(shape.GetLength(0) > 0);
            Assert.True(shape.GetLength(1) > 0);
            
            // Verify the view has a valid symbol
            Assert.True(view.Symbol == 'I' || view.Symbol == 'O' || view.Symbol == 'T' || view.Symbol == 'L');
        }

        [Fact]
        public void CreateRandomPiece_GeneratesAllPieceTypes()
        {
            // Arrange
            var foundPieces = new HashSet<char>();
            
            // Act - Generate many pieces to ensure we get all types
            for (int i = 0; i < 100; i++)
            {
                var (piece, view) = PieceFactory.CreateRandomPiece();
                foundPieces.Add(view.Symbol);
            }

            // Assert - We should have found all piece types
            Assert.Contains('I', foundPieces);
            Assert.Contains('O', foundPieces);
            Assert.Contains('T', foundPieces);
            Assert.Contains('L', foundPieces);
        }

        [Fact]
        public void CreateRandomPiece_MatchesPieceWithView()
        {
            // Act
            for (int i = 0; i < 50; i++)
            {
                var (piece, view) = PieceFactory.CreateRandomPiece();
                
                // Assert - Verify the piece shape matches the expected shape for the symbol
                var shape = piece.GetShape();
                switch (view.Symbol)
                {
                    case 'I':
                        Assert.Equal(1, shape.GetLength(0));
                        Assert.Equal(4, shape.GetLength(1));
                        break;
                    case 'O':
                        Assert.Equal(2, shape.GetLength(0));
                        Assert.Equal(2, shape.GetLength(1));
                        break;
                    case 'T':
                        Assert.Equal(2, shape.GetLength(0));
                        Assert.Equal(3, shape.GetLength(1));
                        break;
                    case 'L':
                        Assert.Equal(3, shape.GetLength(0));
                        Assert.Equal(2, shape.GetLength(1));
                        break;
                }
            }
        }

        [Fact]
        public void AllPieces_HaveCorrectInitialPosition()
        {
            // Arrange & Act
            var pieces = new[]
            {
                PieceFactory.CreateIPiece(),
                PieceFactory.CreateOPiece(),
                PieceFactory.CreateTPiece(),
                PieceFactory.CreateLPiece()
            };

            // Assert
            foreach (var piece in pieces)
            {
                Assert.Equal(0, piece.Position.X);
                Assert.Equal(0, piece.Position.Y);
            }
        }
    }
}