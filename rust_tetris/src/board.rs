use crate::piece::Piece;

/// Represents the game board
#[derive(Debug, Clone)]
pub struct Board {
    grid: Vec<Vec<char>>,
    width: usize,
    height: usize,
    current_piece: Option<Piece>,
}

impl Board {
    /// Creates a new board with the specified dimensions
    pub fn new(width: usize, height: usize) -> Self {
        let mut board = Board {
            grid: vec![vec!['.'; width]; height],
            width,
            height,
            current_piece: None,
        };
        board.clear_board();
        board
    }

    /// Clears the board by filling it with empty spaces
    fn clear_board(&mut self) {
        for i in 0..self.height {
            for j in 0..self.width {
                self.grid[i][j] = '.';
            }
        }
    }

    /// Gets the board width
    pub fn width(&self) -> usize {
        self.width
    }

    /// Gets the board height
    pub fn height(&self) -> usize {
        self.height
    }

    /// Gets a reference to the grid
    pub fn grid(&self) -> &Vec<Vec<char>> {
        &self.grid
    }

    /// Sets the current piece and positions it at the top center
    pub fn set_current_piece(&mut self, mut piece: Piece) {
        let piece_width = piece.shape()[0].len() as i32;
        piece.position_mut().set_x((self.width as i32) / 2 - piece_width / 2);
        piece.position_mut().set_y(0);
        self.current_piece = Some(piece);
    }

    /// Gets a reference to the current piece
    pub fn current_piece(&self) -> Option<&Piece> {
        self.current_piece.as_ref()
    }

    /// Gets a mutable reference to the current piece
    pub fn current_piece_mut(&mut self) -> Option<&mut Piece> {
        self.current_piece.as_mut()
    }

    /// Checks if a piece can be moved by the specified delta
    pub fn can_move_piece(&self, piece: &Piece, delta_x: i32, delta_y: i32) -> bool {
        let shape = piece.shape();
        let pos = piece.position();

        for (i, row) in shape.iter().enumerate() {
            for (j, &cell) in row.iter().enumerate() {
                if cell {
                    let new_x = pos.x + j as i32 + delta_x;
                    let new_y = pos.y + i as i32 + delta_y;

                    // Check bounds
                    if new_x < 0 || new_x >= self.width as i32 || new_y >= self.height as i32 {
                        return false;
                    }

                    // Check collision with placed pieces (only if new_y >= 0)
                    if new_y >= 0 && self.grid[new_y as usize][new_x as usize] != '.' {
                        return false;
                    }
                }
            }
        }
        true
    }

    /// Attempts to rotate a piece in the specified direction
    /// Returns true if rotation was successful
    pub fn can_rotate_piece(&mut self, clockwise: bool) -> bool {
        if self.current_piece.is_none() {
            return false;
        }

        // Clone the piece to test rotation
        let mut test_piece = self.current_piece.as_ref().unwrap().clone();
        
        // Try the rotation
        if clockwise {
            test_piece.rotate_clockwise();
        } else {
            test_piece.rotate_counter_clockwise();
        }

        // Check if the rotation is valid
        let can_rotate = self.can_move_piece(&test_piece, 0, 0);

        // If valid, apply the rotation to the actual piece
        if can_rotate {
            if let Some(piece) = self.current_piece_mut() {
                if clockwise {
                    piece.rotate_clockwise();
                } else {
                    piece.rotate_counter_clockwise();
                }
            }
        }

        can_rotate
    }

    /// Places a piece permanently on the board
    pub fn place_piece(&mut self, piece: &Piece, symbol: char) {
        let shape = piece.shape();
        let pos = piece.position();

        for (i, row) in shape.iter().enumerate() {
            for (j, &cell) in row.iter().enumerate() {
                if cell {
                    let x = pos.x + j as i32;
                    let y = pos.y + i as i32;
                    if y >= 0 && y < self.height as i32 && x >= 0 && x < self.width as i32 {
                        self.grid[y as usize][x as usize] = symbol;
                    }
                }
            }
        }
    }

    /// Clears complete lines and returns the number of lines cleared
    pub fn clear_complete_lines(&mut self) -> usize {
        let mut lines_cleared = 0;

        let mut i = self.height - 1;
        loop {
            let mut is_complete = true;

            // Check if line is complete
            for j in 0..self.width {
                if self.grid[i][j] == '.' {
                    is_complete = false;
                    break;
                }
            }

            if is_complete {
                // Move all lines above down by one
                for k in (1..=i).rev() {
                    for j in 0..self.width {
                        self.grid[k][j] = self.grid[k - 1][j];
                    }
                }

                // Clear the top line
                for j in 0..self.width {
                    self.grid[0][j] = '.';
                }

                lines_cleared += 1;
                // Don't decrement i, check the same line again
            } else {
                if i == 0 {
                    break;
                }
                i -= 1;
            }
        }

        lines_cleared
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::piece::{Piece, PieceType};

    #[test]
    fn test_board_creation() {
        let board = Board::new(10, 20);
        assert_eq!(board.width(), 10);
        assert_eq!(board.height(), 20);
        assert_eq!(board.grid().len(), 20);
        assert_eq!(board.grid()[0].len(), 10);
        
        // Check that board is empty
        for row in board.grid() {
            for &cell in row {
                assert_eq!(cell, '.');
            }
        }
    }

    #[test]
    fn test_set_current_piece() {
        let mut board = Board::new(10, 20);
        let piece = Piece::new(PieceType::I);
        board.set_current_piece(piece);
        
        assert!(board.current_piece().is_some());
        let current = board.current_piece().unwrap();
        assert_eq!(current.piece_type(), PieceType::I);
    }

    #[test]
    fn test_can_move_piece() {
        let board = Board::new(10, 20);
        let piece = Piece::new(PieceType::O);
        
        // Should be able to move within bounds
        assert!(board.can_move_piece(&piece, 1, 1));
        assert!(board.can_move_piece(&piece, 0, 1));
        
        // Should not be able to move out of bounds
        assert!(!board.can_move_piece(&piece, -1, 0));
        assert!(!board.can_move_piece(&piece, 10, 0));
    }

    #[test]
    fn test_place_piece() {
        let mut board = Board::new(10, 20);
        let mut piece = Piece::new(PieceType::O);
        piece.position_mut().set_x(0);
        piece.position_mut().set_y(0);
        
        board.place_piece(&piece, 'O');
        
        // Check that piece was placed
        assert_eq!(board.grid()[0][0], 'O');
        assert_eq!(board.grid()[0][1], 'O');
        assert_eq!(board.grid()[1][0], 'O');
        assert_eq!(board.grid()[1][1], 'O');
    }

    #[test]
    fn test_clear_complete_lines() {
        let mut board = Board::new(3, 3);
        
        // Fill bottom line completely
        for j in 0..3 {
            board.grid[2][j] = 'X';
        }
        
        let lines_cleared = board.clear_complete_lines();
        assert_eq!(lines_cleared, 1);
        
        // Check that top line is now empty and others moved down
        for j in 0..3 {
            assert_eq!(board.grid[2][j], '.');
        }
    }
}