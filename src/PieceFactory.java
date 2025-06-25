public class PieceFactory {
    public static Piece createIPiece() {
        boolean[][] shape = {
                { true, true, true, true }
        };
        return new Piece(shape, 'I');
    }

    public static Piece createOPiece() {
        boolean[][] shape = {
                { true, true },
                { true, true }
        };
        return new Piece(shape, 'O');
    }

    public static Piece createTPiece() {
        boolean[][] shape = {
                { false, true, false },
                { true, true, true }
        };
        return new Piece(shape, 'T');
    }

    public static Piece createLPiece() {
        boolean[][] shape = {
                { true, false },
                { true, false },
                { true, true }
        };
        return new Piece(shape, 'L');
    }
        public static Piece createJPiece() {
        boolean[][] shape = {
                { false, true },
                { false, true },
                { true, true }
        };
        return new Piece(shape, 'J');
    }
        public static Piece createSPiece() {
        boolean[][] shape = {
                { false, true, true },
                { true, true, false }
        };
        return new Piece(shape, 'S');
    }
        public static Piece createZPiece() {
        boolean[][] shape = {
                { true, true, false },
                { false, true, true }
        };
        return new Piece(shape, 'Z');
    }
}