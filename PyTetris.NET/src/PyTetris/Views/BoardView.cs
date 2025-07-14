using PyTetris.Models;
using PyTetris.Services;

namespace PyTetris.Views
{
    /// <summary>
    /// Handles the display of the game board
    /// </summary>
    public class BoardView
    {
        private readonly ConsoleService console;

        public BoardView(ConsoleService console)
        {
            this.console = console;
        }

        public void Display(Board board, PieceView? currentPieceView)
        {
            int width = board.Width;
            int height = board.Height;
            char[,] displayGrid = new char[height, width];

            var grid = board.GetGrid();
            for (int i = 0; i < height; i++)
            {
                for (int j = 0; j < width; j++)
                {
                    displayGrid[i, j] = grid[i, j];
                }
            }

            if (currentPieceView != null && board.CurrentPiece != null)
            {
                currentPieceView.Render(board.CurrentPiece, displayGrid);
            }

            console.ClearScreen();
            console.WriteLine("<!" + new string('=', ((width + 2) * 2) - 1) + "!>");
            
            for (int i = 0; i < height; i++)
            {
                console.Write("<!  ");
                for (int j = 0; j < width; j++)
                {
                    console.Write(displayGrid[i, j] + " ");
                }
                console.WriteLine(" !>");
            }
            
            console.WriteLine("<!" + new string('=', ((width + 2) * 2) - 1) + "!>");
            console.WriteLine(new string('\\', width + 4).Replace("\\", "\\/"));
        }
    }
}