namespace PyTetris.Models
{
    /// <summary>
    /// Represents a Tetris piece with shape and position
    /// </summary>
    public class Piece
    {
        private bool[,] shape;
        public Position Position { get; private set; }

        public Piece(bool[,] shape)
        {
            this.shape = shape;
            Position = new Position(0, 0);
        }

        public bool[,] GetShape()
        {
            return shape;
        }

        public int GetWidth()
        {
            return shape.GetLength(1);
        }

        public int GetHeight()
        {
            return shape.GetLength(0);
        }

        public void MoveDown()
        {
            Position.Y++;
        }

        public void MoveLeft()
        {
            Position.X--;
        }

        public void MoveRight()
        {
            Position.X++;
        }

        public void Rotate()
        {
            RotateClockwise();
        }

        public void RotateClockwise()
        {
            int rows = shape.GetLength(0);
            int cols = shape.GetLength(1);
            bool[,] rotated = new bool[cols, rows];

            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < cols; j++)
                {
                    rotated[j, rows - 1 - i] = shape[i, j];
                }
            }
            shape = rotated;
        }

        public void RotateCounterClockwise()
        {
            int rows = shape.GetLength(0);
            int cols = shape.GetLength(1);
            bool[,] rotated = new bool[cols, rows];

            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < cols; j++)
                {
                    rotated[cols - 1 - j, i] = shape[i, j];
                }
            }
            shape = rotated;
        }

        public Piece Clone()
        {
            int rows = shape.GetLength(0);
            int cols = shape.GetLength(1);
            bool[,] clonedShape = new bool[rows, cols];

            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < cols; j++)
                {
                    clonedShape[i, j] = shape[i, j];
                }
            }

            var clone = new Piece(clonedShape);
            clone.Position.X = Position.X;
            clone.Position.Y = Position.Y;
            return clone;
        }
    }
}