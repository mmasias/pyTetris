use crate::board::Board;
use crate::console::Console;
use crate::piece_view::PieceView;
use std::io;

/// Handles the display of the game board
pub struct BoardView {
    console: Console,
}

impl BoardView {
    /// Creates a new board view with the given console
    pub fn new(console: Console) -> Self {
        BoardView { console }
    }

    /// Displays the board and current piece
    pub fn display(&self, board: &Board, current_piece_view: Option<&PieceView>) -> Result<(), io::Error> {
        let width = board.width();
        let height = board.height();
        let mut display_grid = vec![vec!['.'; width]; height];

        // Copy the board grid
        let grid = board.grid();
        for i in 0..height {
            for j in 0..width {
                display_grid[i][j] = grid[i][j];
            }
        }

        // Render the current piece if it exists
        if let (Some(piece_view), Some(current_piece)) = (current_piece_view, board.current_piece()) {
            piece_view.render(current_piece, &mut display_grid);
        }

        // Clear screen and display the board
        self.console.clear_screen()?;
        
        // Top border
        let border = "=".repeat((width + 2) * 2 - 1);
        self.console.writeln(&format!("<!{border}!>"))?;
        
        // Board content
        for row in display_grid.iter().take(height) {
            self.console.write("<!  ")?;
            for &cell in row {
                self.console.write(&format!("{cell} "))?;
            }
            self.console.writeln(" !>")?;
        }
        
        // Bottom border
        self.console.writeln(&format!("<!{border}!>"))?;
        
        // Bottom decoration
        let bottom_decoration = "\\/".repeat(width + 4);
        self.console.writeln(&bottom_decoration)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::Board;
    use crate::piece::{Piece, PieceType};
    use crate::piece_view::PieceView;

    #[test]
    fn test_board_view_creation() {
        let console = Console::new();
        let board_view = BoardView::new(console);
        // Just test that creation works
    }

    #[test]
    fn test_display_empty_board() {
        let console = Console::new();
        let board_view = BoardView::new(console);
        let board = Board::new(5, 5);
        
        // This would normally output to console, so we just test it doesn't panic
        let result = board_view.display(&board, None);
        assert!(result.is_ok());
    }

    #[test]
    fn test_display_board_with_piece() {
        let console = Console::new();
        let board_view = BoardView::new(console);
        let mut board = Board::new(5, 5);
        
        let piece = Piece::new(PieceType::O);
        let piece_view = PieceView::from_piece_type(PieceType::O);
        board.set_current_piece(piece);
        
        // This would normally output to console, so we just test it doesn't panic
        let result = board_view.display(&board, Some(&piece_view));
        assert!(result.is_ok());
    }
}