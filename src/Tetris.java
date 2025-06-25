public class Tetris {
    private Board board;
    private BoardView boardView;
    private boolean gameRunning;
    private int score;
    private Console console;
    private PieceView currentPieceView;

    public Tetris(int width, int height) {
        console = new Console();
        board = new Board(width, height);
        boardView = new BoardView(console);
        gameRunning = true;
        score = 0;
    }

    public void spawnNewPiece() {
        Piece[] pieces = {
                PieceFactory.createIPiece(),
                PieceFactory.createOPiece(),
                PieceFactory.createTPiece(),
                PieceFactory.createLPiece()
        };

        PieceView[] pieceViews = {
                PieceFactory.createIPieceView(),
                PieceFactory.createOPieceView(),
                PieceFactory.createTPieceView(),
                PieceFactory.createLPieceView()
        };

        int randomIndex = (int) (Math.random() * pieces.length);
        board.setCurrentPiece(pieces[randomIndex]);
        currentPieceView = pieceViews[randomIndex];
    }

    private void processUserInput(String input) {
        if (input.length() != 0) {
            char command = input.charAt(0);
            Piece currentPiece = board.getCurrentPiece();

            switch (command) {
                case '4' -> {
                    if (board.canMovePiece(currentPiece, -1, 0))
                        currentPiece.moveLeft();
                }
                case '6' -> {
                    if (board.canMovePiece(currentPiece, 1, 0))
                        currentPiece.moveRight();
                }
                case '7' -> board.canRotatePiece(currentPiece, false);
                case '9' -> board.canRotatePiece(currentPiece, true);
            }
        }
    }

    public void gameLoop() {
        spawnNewPiece();
        while (gameRunning) {
            boardView.display(board, currentPieceView);
            String input = console.readString("Score: " + score + "\n\nComando (4=izq, 6=der, 7=rotar↺, 9=rotar↻): ");
            processUserInput(input);

            Piece currentPiece = board.getCurrentPiece();

            if (board.canMovePiece(currentPiece, 0, 1)) {
                currentPiece.moveDown();
            } else {
                board.placePiece(currentPiece, currentPieceView.getSymbol());

                int linesCleared = board.clearCompleteLines();
                score += linesCleared * 100;

                if (linesCleared > 0) {
                    console.writeln("¡" + linesCleared + " línea(s) eliminada(s)!");
                    console.readString("Presiona Enter para continuar...");
                }

                spawnNewPiece();

                if (!board.canMovePiece(board.getCurrentPiece(), 0, 0)) {
                    gameRunning = false;
                    console.writeln("¡GAME OVER!");
                    console.writeln("Puntuación final: " + score);
                }
            }
        }
    }
}