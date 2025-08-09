using PyTetris.Models;
using PyTetris.Views;

namespace PyTetris.Factories
{
    /// <summary>
    /// Factory class for creating different types of Tetris pieces
    /// </summary>
    public static class PieceFactory
    {
        public static Piece CreateIPiece()
        {
            bool[,] shape = {
                { true, true, true, true }
            };
            return new Piece(shape);
        }

        public static Piece CreateOPiece()
        {
            bool[,] shape = {
                { true, true },
                { true, true }
            };
            return new Piece(shape);
        }

        public static Piece CreateTPiece()
        {
            bool[,] shape = {
                { false, true, false },
                { true, true, true }
            };
            return new Piece(shape);
        }

        public static Piece CreateLPiece()
        {
            bool[,] shape = {
                { true, false },
                { true, false },
                { true, true }
            };
            return new Piece(shape);
        }

        public static PieceView CreateIPieceView()
        {
            return new PieceView('I');
        }

        public static PieceView CreateOPieceView()
        {
            return new PieceView('O');
        }

        public static PieceView CreateTPieceView()
        {
            return new PieceView('T');
        }

        public static PieceView CreateLPieceView()
        {
            return new PieceView('L');
        }

        public static (Piece piece, PieceView view) CreateRandomPiece()
        {
            var random = new Random();
            int pieceType = random.Next(4);

            return pieceType switch
            {
                0 => (CreateIPiece(), CreateIPieceView()),
                1 => (CreateOPiece(), CreateOPieceView()),
                2 => (CreateTPiece(), CreateTPieceView()),
                3 => (CreateLPiece(), CreateLPieceView()),
                _ => (CreateIPiece(), CreateIPieceView())
            };
        }
    }
}