using PyTetris.Models;
using PyTetris.Views;
using PyTetris.Factories;

namespace PyTetris.Services
{
    /// <summary>
    /// Main game service that manages the Tetris game logic
    /// </summary>
    public class TetrisGame
    {
        private readonly Board board;
        private readonly BoardView boardView;
        private readonly ConsoleService console;
        private bool gameRunning;
        private int score;
        private PieceView? currentPieceView;

        public TetrisGame(int width, int height)
        {
            console = new ConsoleService();
            board = new Board(width, height);
            boardView = new BoardView(console);
            gameRunning = true;
            score = 0;
        }

        public Board GetBoard() => board;
        public int GetScore() => score;
        public bool IsGameRunning() => gameRunning;

        public void SpawnNewPiece()
        {
            var (piece, view) = PieceFactory.CreateRandomPiece();
            board.SetCurrentPiece(piece);
            currentPieceView = view;
        }

        private void ProcessUserInput(string input)
        {
            if (string.IsNullOrEmpty(input) || board.CurrentPiece == null)
                return;

            char command = input[0];
            var currentPiece = board.CurrentPiece;

            switch (command)
            {
                case '4':
                    if (board.CanMovePiece(currentPiece, -1, 0))
                        currentPiece.MoveLeft();
                    break;
                case '6':
                    if (board.CanMovePiece(currentPiece, 1, 0))
                        currentPiece.MoveRight();
                    break;
                case '7':
                    board.CanRotatePiece(currentPiece, false);
                    break;
                case '9':
                    board.CanRotatePiece(currentPiece, true);
                    break;
            }
        }

        public void GameLoop()
        {
            SpawnNewPiece();
            
            while (gameRunning)
            {
                boardView.Display(board, currentPieceView);
                string input = console.ReadString($"Score: {score}\n\nComando (4=izq, 6=der, 7=rotar↺, 9=rotar↻): ");
                ProcessUserInput(input);

                var currentPiece = board.CurrentPiece;
                if (currentPiece == null) continue;

                if (board.CanMovePiece(currentPiece, 0, 1))
                {
                    currentPiece.MoveDown();
                }
                else
                {
                    // Place piece on board
                    board.PlacePiece(currentPiece, currentPieceView?.Symbol ?? '#');

                    // Clear complete lines
                    int linesCleared = board.ClearCompleteLines();
                    score += linesCleared * 100;

                    if (linesCleared > 0)
                    {
                        console.WriteLine($"¡{linesCleared} línea(s) eliminada(s)!");
                        console.ReadString("Presiona Enter para continuar...");
                    }

                    // Spawn new piece
                    SpawnNewPiece();

                    // Check game over
                    if (board.CurrentPiece != null && !board.CanMovePiece(board.CurrentPiece, 0, 0))
                    {
                        gameRunning = false;
                        console.WriteLine("¡GAME OVER!");
                        console.WriteLine($"Puntuación final: {score}");
                    }
                }
            }
        }
    }
}