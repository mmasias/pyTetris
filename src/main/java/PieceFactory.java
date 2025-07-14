public class PieceFactory {
    public static Piece createIPiece() {
        boolean[][] shape = {
                { true, true, true, true }
        };
        return new Piece(shape);
    }

    public static Piece createOPiece() {
        boolean[][] shape = {
                { true, true },
                { true, true }
        };
        return new Piece(shape);
    }

    public static Piece createTPiece() {
        boolean[][] shape = {
                { false, true, false },
                { true, true, true }
        };
        return new Piece(shape);
    }

    public static Piece createLPiece() {
        boolean[][] shape = {
                { true, false },
                { true, false },
                { true, true }
        };
        return new Piece(shape);
    }

    public static PieceView createIPieceView() {
        return new PieceView('I');
    }

    public static PieceView createOPieceView() {
        return new PieceView('O');
    }

    public static PieceView createTPieceView() {
        return new PieceView('T');
    }

    public static PieceView createLPieceView() {
        return new PieceView('L');
    }
}