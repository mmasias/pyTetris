class PieceView {
    private char symbol;
    private Piece piece;

    public PieceView(Piece piece, char symbol) {
        this.piece = piece;
        this.symbol = symbol;
    }

    public char getSymbol() {
        return symbol;
    }

    public void setSymbol(char symbol) {
        this.symbol = symbol;
    }

    public Piece getPiece() {
        return piece;
    }

    public void render(char[][] displayGrid) {
        boolean[][] shape = piece.getShape();
        Position pos = piece.getPosition();

        for (int i = 0; i < shape.length; i++) {
            for (int j = 0; j < shape[i].length; j++) {
                if (shape[i][j]) {
                    int x = pos.getX() + j;
                    int y = pos.getY() + i;
                    if (y >= 0 && y < displayGrid.length && x >= 0 && x < displayGrid[0].length) {
                        displayGrid[y][x] = symbol;
                    }
                }
            }
        }
    }
}