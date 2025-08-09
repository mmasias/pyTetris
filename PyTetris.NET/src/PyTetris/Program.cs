using PyTetris.Services;

namespace PyTetris
{
    /// <summary>
    /// Main entry point for the Tetris game
    /// </summary>
    class Program
    {
        static void Main(string[] args)
        {
            var game = new TetrisGame(10, 20);
            game.GameLoop();
        }
    }
}
