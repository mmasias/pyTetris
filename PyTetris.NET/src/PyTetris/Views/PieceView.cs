using PyTetris.Models;

namespace PyTetris.Views
{
    /// <summary>
    /// Represents the visual representation of a Tetris piece
    /// </summary>
    public class PieceView
    {
        public char Symbol { get; set; }

        public PieceView(char symbol)
        {
            Symbol = symbol;
        }

        public void Render(Piece piece, char[,] displayGrid)
        {
            var shape = piece.GetShape();
            var pos = piece.Position;

            for (int i = 0; i < shape.GetLength(0); i++)
            {
                for (int j = 0; j < shape.GetLength(1); j++)
                {
                    if (shape[i, j])
                    {
                        int x = pos.X + j;
                        int y = pos.Y + i;
                        if (y >= 0 && y < displayGrid.GetLength(0) && x >= 0 && x < displayGrid.GetLength(1))
                        {
                            displayGrid[y, x] = Symbol;
                        }
                    }
                }
            }
        }
    }
}