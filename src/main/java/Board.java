public class Board {
    private char[][] grid;
    private int width;
    private int height;
    private Piece currentPiece;

    public Board(int width, int height) {
        this.width = width;
        this.height = height;
        this.grid = new char[height][width];
        clearBoard();
    }

    private void clearBoard() {
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                grid[i][j] = '.';
            }
        }
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    public char[][] getGrid() {
        return grid;
    }

    public void setCurrentPiece(Piece piece) {
        this.currentPiece = piece;
        piece.getPosition().setX(width / 2 - piece.getShape()[0].length / 2);
        piece.getPosition().setY(0);
    }

    public Piece getCurrentPiece() {
        return currentPiece;
    }

    public boolean canMovePiece(Piece piece, int deltaX, int deltaY) {
        boolean[][] shape = piece.getShape();
        Position pos = piece.getPosition();

        for (int i = 0; i < shape.length; i++) {
            for (int j = 0; j < shape[i].length; j++) {
                if (shape[i][j]) {
                    int newX = pos.getX() + j + deltaX;
                    int newY = pos.getY() + i + deltaY;

                    if (newX < 0 || newX >= width || newY >= height) {
                        return false;
                    }

                    if (newY >= 0 && grid[newY][newX] != '.') {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    public boolean canRotatePiece(Piece piece, boolean clockwise) {
        if (clockwise) {
            piece.rotateClockwise();
        } else {
            piece.rotateCounterClockwise();
        }

        boolean canRotate = canMovePiece(piece, 0, 0);

        if (!canRotate) {
            // Restore original by doing one rotation in opposite direction
            if (clockwise) {
                piece.rotateCounterClockwise();
            } else {
                piece.rotateClockwise();
            }
        }

        return canRotate;
    }

    public void placePiece(Piece piece, char symbol) {
        boolean[][] shape = piece.getShape();
        Position pos = piece.getPosition();

        for (int i = 0; i < shape.length; i++) {
            for (int j = 0; j < shape[i].length; j++) {
                if (shape[i][j]) {
                    int x = pos.getX() + j;
                    int y = pos.getY() + i;
                    if (y >= 0 && y < height && x >= 0 && x < width) {
                        grid[y][x] = symbol;
                    }
                }
            }
        }
    }

    public int clearCompleteLines() {
        int linesCleared = 0;

        for (int i = height - 1; i >= 0; i--) {
            boolean isComplete = true;

            for (int j = 0; j < width && isComplete; j++) {
                if (grid[i][j] == '.') {
                    isComplete = false;
                }
            }

            if (isComplete) {
                for (int k = i; k > 0; k--) {
                    for (int j = 0; j < width; j++) {
                        grid[k][j] = grid[k - 1][j];
                    }
                }

                for (int j = 0; j < width; j++) {
                    grid[0][j] = '.';
                }

                linesCleared++;
                i++;
            }
        }

        return linesCleared;
    }
}