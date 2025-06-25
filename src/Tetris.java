public class Tetris {
    private Board board;
    private boolean gameRunning;
    private int score;
    private Console console;

    public Tetris(int width, int height) {
        board = new Board(width, height, new Console());
        gameRunning = true;
        score = 0;
        console = new Console();
    }

    public void spawnNewPiece() {
        PieceView[] pieceViews = {
                PieceFactory.createIPieceView(),
                PieceFactory.createOPieceView(),
                PieceFactory.createTPieceView(),
                PieceFactory.createLPieceView()
        };

        int randomIndex = (int) (Math.random() * pieceViews.length);
        board.setCurrentPieceView(pieceViews[randomIndex]);
    }

    private void processUserInput(String input) {
        if (input.length() != 0) {
            char command = input.charAt(0);
            PieceView currentPieceView = board.getCurrentPieceView();
            Piece currentPiece = currentPieceView.getPiece();

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
            board.display();
            String input = console.readString("Score: " + score + "\n\nComando (4=izq, 6=der, 7=rotar↺, 9=rotar↻): ");
            processUserInput(input);

            PieceView currentPieceView = board.getCurrentPieceView();
            Piece currentPiece = currentPieceView.getPiece();

            if (board.canMovePiece(currentPiece, 0, 1)) {
                currentPiece.moveDown();
            } else {
                board.placePieceView(currentPieceView);

                int linesCleared = board.clearCompleteLines();
                score += linesCleared * 100;

                if (linesCleared > 0) {
                    console.writeln("¡" + linesCleared + " línea(s) eliminada(s)!");
                    console.readString("Presiona Enter para continuar...");
                }

                spawnNewPiece();

                PieceView newPieceView = board.getCurrentPieceView();
                Piece newPiece = newPieceView.getPiece();
                
                if (!board.canMovePiece(newPiece, 0, 0)) {
                    gameRunning = false;
                    console.writeln("¡GAME OVER!");
                    console.writeln("Puntuación final: " + score);
                }
            }
        }
    }
}