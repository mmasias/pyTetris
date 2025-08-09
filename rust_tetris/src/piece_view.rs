use crate::piece::{Piece, PieceType};

/// Handles rendering of individual pieces
#[derive(Debug, Clone)]
pub struct PieceView {
    symbol: char,
}

impl PieceView {
    /// Creates a new piece view with the given symbol
    pub fn new(symbol: char) -> Self {
        PieceView { symbol }
    }

    /// Creates a piece view from a piece type
    pub fn from_piece_type(piece_type: PieceType) -> Self {
        PieceView::new(piece_type.symbol())
    }

    /// Gets the symbol for this piece view
    pub fn symbol(&self) -> char {
        self.symbol
    }

    /// Sets the symbol for this piece view
    #[allow(dead_code)]
    pub fn set_symbol(&mut self, symbol: char) {
        self.symbol = symbol;
    }

    /// Renders the piece onto the display grid
    pub fn render(&self, piece: &Piece, display_grid: &mut [Vec<char>]) {
        let shape = piece.shape();
        let pos = piece.position();

        for (i, row) in shape.iter().enumerate() {
            for (j, &cell) in row.iter().enumerate() {
                if cell {
                    let x = pos.x + j as i32;
                    let y = pos.y + i as i32;
                    
                    if y >= 0 
                        && y < display_grid.len() as i32 
                        && x >= 0 
                        && x < display_grid[0].len() as i32 
                    {
                        display_grid[y as usize][x as usize] = self.symbol;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::piece::Piece;
    use crate::position::Position;

    #[test]
    fn test_piece_view_creation() {
        let view = PieceView::new('X');
        assert_eq!(view.symbol(), 'X');
    }

    #[test]
    fn test_piece_view_from_type() {
        let view = PieceView::from_piece_type(PieceType::I);
        assert_eq!(view.symbol(), 'I');
        
        let view = PieceView::from_piece_type(PieceType::O);
        assert_eq!(view.symbol(), 'O');
    }

    #[test]
    fn test_piece_view_render() {
        let mut piece = Piece::new(PieceType::O);
        piece.position_mut().set_x(1);
        piece.position_mut().set_y(1);
        
        let view = PieceView::from_piece_type(PieceType::O);
        let mut display_grid = vec![vec!['.'; 5]; 5];
        
        view.render(&piece, &mut display_grid);
        
        // Check that the O piece was rendered correctly
        assert_eq!(display_grid[1][1], 'O');
        assert_eq!(display_grid[1][2], 'O');
        assert_eq!(display_grid[2][1], 'O');
        assert_eq!(display_grid[2][2], 'O');
        
        // Check that other cells are unchanged
        assert_eq!(display_grid[0][0], '.');
        assert_eq!(display_grid[3][3], '.');
    }

    #[test]
    fn test_piece_view_symbol_setter() {
        let mut view = PieceView::new('A');
        assert_eq!(view.symbol(), 'A');
        
        view.set_symbol('B');
        assert_eq!(view.symbol(), 'B');
    }
}