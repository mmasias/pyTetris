public class PieceFactory {
    public static PieceView createIPieceView() {
        boolean[][] shape = {
                { true, true, true, true }
        };
        Piece piece = new Piece(shape);
        return new PieceView(piece, 'I');
    }

    public static PieceView createOPieceView() {
        boolean[][] shape = {
                { true, true },
                { true, true }
        };
        Piece piece = new Piece(shape);
        return new PieceView(piece, 'O');
    }

    public static PieceView createTPieceView() {
        boolean[][] shape = {
                { false, true, false },
                { true, true, true }
        };
        Piece piece = new Piece(shape);
        return new PieceView(piece, 'T');
    }

    public static PieceView createLPieceView() {
        boolean[][] shape = {
                { true, false },
                { true, false },
                { true, true }
        };
        Piece piece = new Piece(shape);
        return new PieceView(piece, 'L');
    }
}