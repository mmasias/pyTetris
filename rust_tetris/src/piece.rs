use crate::position::Position;

/// Represents different types of Tetris pieces
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceType {
    I,
    O,
    T,
    L,
}

impl PieceType {
    /// Gets the symbol character for this piece type
    pub fn symbol(&self) -> char {
        match self {
            PieceType::I => 'I',
            PieceType::O => 'O',
            PieceType::T => 'T',
            PieceType::L => 'L',
        }
    }

    /// Gets the initial shape for this piece type
    pub fn initial_shape(&self) -> Vec<Vec<bool>> {
        match self {
            PieceType::I => vec![
                vec![true, true, true, true]
            ],
            PieceType::O => vec![
                vec![true, true],
                vec![true, true]
            ],
            PieceType::T => vec![
                vec![false, true, false],
                vec![true, true, true]
            ],
            PieceType::L => vec![
                vec![true, false],
                vec![true, false],
                vec![true, true]
            ],
        }
    }
}

/// Represents a Tetris piece with its shape and position
#[derive(Debug, Clone)]
pub struct Piece {
    shape: Vec<Vec<bool>>,
    position: Position,
    piece_type: PieceType,
}

impl Piece {
    /// Creates a new piece of the given type
    pub fn new(piece_type: PieceType) -> Self {
        Piece {
            shape: piece_type.initial_shape(),
            position: Position::new(0, 0),
            piece_type,
        }
    }

    /// Gets the current shape of the piece
    pub fn shape(&self) -> &Vec<Vec<bool>> {
        &self.shape
    }

    /// Gets the current position of the piece
    pub fn position(&self) -> &Position {
        &self.position
    }

    /// Gets a mutable reference to the position
    pub fn position_mut(&mut self) -> &mut Position {
        &mut self.position
    }

    /// Gets the piece type
    #[allow(dead_code)]
    pub fn piece_type(&self) -> PieceType {
        self.piece_type
    }

    /// Moves the piece down by one unit
    pub fn move_down(&mut self) {
        self.position.y += 1;
    }

    /// Moves the piece left by one unit
    pub fn move_left(&mut self) {
        self.position.x -= 1;
    }

    /// Moves the piece right by one unit
    pub fn move_right(&mut self) {
        self.position.x += 1;
    }

    /// Rotates the piece clockwise
    pub fn rotate_clockwise(&mut self) {
        let rows = self.shape.len();
        let cols = self.shape[0].len();
        let mut rotated = vec![vec![false; rows]; cols];

        for i in 0..rows {
            for j in 0..cols {
                rotated[j][rows - 1 - i] = self.shape[i][j];
            }
        }
        self.shape = rotated;
    }

    /// Rotates the piece counter-clockwise
    pub fn rotate_counter_clockwise(&mut self) {
        let rows = self.shape.len();
        let cols = self.shape[0].len();
        let mut rotated = vec![vec![false; rows]; cols];

        for i in 0..rows {
            for j in 0..cols {
                rotated[cols - 1 - j][i] = self.shape[i][j];
            }
        }
        self.shape = rotated;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_piece_creation() {
        let piece = Piece::new(PieceType::I);
        assert_eq!(piece.piece_type(), PieceType::I);
        assert_eq!(piece.position().x, 0);
        assert_eq!(piece.position().y, 0);
        assert_eq!(piece.shape().len(), 1);
        assert_eq!(piece.shape()[0].len(), 4);
    }

    #[test]
    fn test_piece_movement() {
        let mut piece = Piece::new(PieceType::O);
        
        piece.move_down();
        assert_eq!(piece.position().y, 1);
        
        piece.move_left();
        assert_eq!(piece.position().x, -1);
        
        piece.move_right();
        piece.move_right();
        assert_eq!(piece.position().x, 1);
    }

    #[test]
    fn test_piece_rotation() {
        let mut piece = Piece::new(PieceType::I);
        let original_shape = piece.shape().clone();
        
        // Rotate clockwise
        piece.rotate_clockwise();
        assert_ne!(piece.shape(), &original_shape);
        assert_eq!(piece.shape().len(), 4);
        assert_eq!(piece.shape()[0].len(), 1);
        
        // Rotate back counter-clockwise
        piece.rotate_counter_clockwise();
        assert_eq!(piece.shape(), &original_shape);
    }

    #[test]
    fn test_piece_type_symbols() {
        assert_eq!(PieceType::I.symbol(), 'I');
        assert_eq!(PieceType::O.symbol(), 'O');
        assert_eq!(PieceType::T.symbol(), 'T');
        assert_eq!(PieceType::L.symbol(), 'L');
    }
}