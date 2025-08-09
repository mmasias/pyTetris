namespace PyTetris.Models
{
    /// <summary>
    /// Represents the game board with grid management and piece collision detection
    /// </summary>
    public class Board
    {
        private char[,] grid;
        public int Width { get; private set; }
        public int Height { get; private set; }
        public Piece? CurrentPiece { get; private set; }

        public Board(int width, int height)
        {
            Width = width;
            Height = height;
            grid = new char[height, width];
            ClearBoard();
        }

        private void ClearBoard()
        {
            for (int i = 0; i < Height; i++)
            {
                for (int j = 0; j < Width; j++)
                {
                    grid[i, j] = '.';
                }
            }
        }

        public char[,] GetGrid()
        {
            return grid;
        }

        public char GetCell(int x, int y)
        {
            if (x >= 0 && x < Width && y >= 0 && y < Height)
            {
                return grid[y, x];
            }
            return ' '; // Out of bounds
        }

        public void SetCurrentPiece(Piece piece)
        {
            CurrentPiece = piece;
            piece.Position.X = Width / 2 - piece.GetWidth() / 2;
            piece.Position.Y = 0;
        }

        public bool CanMovePiece(Piece piece, int deltaX, int deltaY)
        {
            var shape = piece.GetShape();
            var pos = piece.Position;

            for (int i = 0; i < shape.GetLength(0); i++)
            {
                for (int j = 0; j < shape.GetLength(1); j++)
                {
                    if (shape[i, j])
                    {
                        int newX = pos.X + j + deltaX;
                        int newY = pos.Y + i + deltaY;

                        if (newX < 0 || newX >= Width || newY >= Height)
                        {
                            return false;
                        }

                        if (newY >= 0 && grid[newY, newX] != '.')
                        {
                            return false;
                        }
                    }
                }
            }
            return true;
        }

        public bool CanRotatePiece(Piece piece, bool clockwise)
        {
            var originalPiece = piece.Clone();

            if (clockwise)
            {
                piece.RotateClockwise();
            }
            else
            {
                piece.RotateCounterClockwise();
            }

            bool canRotate = CanMovePiece(piece, 0, 0);

            if (!canRotate)
            {
                // Restore original rotation
                piece = originalPiece;
                if (CurrentPiece == piece)
                {
                    CurrentPiece = originalPiece;
                }
            }

            return canRotate;
        }

        public void PlacePiece(Piece piece, char symbol)
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
                        if (y >= 0 && y < Height && x >= 0 && x < Width)
                        {
                            grid[y, x] = symbol;
                        }
                    }
                }
            }
        }

        public int ClearCompleteLines()
        {
            int linesCleared = 0;

            for (int i = Height - 1; i >= 0; i--)
            {
                bool isComplete = true;

                for (int j = 0; j < Width && isComplete; j++)
                {
                    if (grid[i, j] == '.')
                    {
                        isComplete = false;
                    }
                }

                if (isComplete)
                {
                    // Move all lines above down by one
                    for (int k = i; k > 0; k--)
                    {
                        for (int j = 0; j < Width; j++)
                        {
                            grid[k, j] = grid[k - 1, j];
                        }
                    }

                    // Clear the top line
                    for (int j = 0; j < Width; j++)
                    {
                        grid[0, j] = '.';
                    }

                    linesCleared++;
                    i++; // Check the same line again
                }
            }

            return linesCleared;
        }
    }
}