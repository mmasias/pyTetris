public class BoardView {
    private Console console;

    public BoardView(Console console) {
        this.console = console;
    }

    public void display(Board board, PieceView currentPieceView) {
        int width = board.getWidth();
        int height = board.getHeight();
        char[][] displayGrid = new char[height][width];
        
        char[][] grid = board.getGrid();
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                displayGrid[i][j] = grid[i][j];
            }
        }

        if (currentPieceView != null && board.getCurrentPiece() != null) {
            currentPieceView.render(board.getCurrentPiece(), displayGrid);
        }

        console.clearScreen();
        console.writeln("<!" + "=".repeat(((width + 2) * 2) - 1) + "!>");
        for (int i = 0; i < height; i++) {
            console.write("<!  ");
            for (int j = 0; j < width; j++) {
                console.write(displayGrid[i][j] + " ");
            }
            console.writeln(" !>");
        }
        console.writeln("<!" + "=".repeat(((width + 2) * 2) - 1) + "!>");
        console.writeln("\\/".repeat(width + 4));
    }
}