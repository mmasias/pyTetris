use crate::board::Board;
use crate::board_view::BoardView;
use crate::console::{Console, ForegroundColor};
use crate::piece::{Piece, PieceType};
use crate::piece_view::PieceView;
use rand::Rng;
use std::io;

/// Main Tetris game controller
pub struct Tetris {
    board: Board,
    board_view: BoardView,
    game_running: bool,
    score: usize,
    console: Console,
    current_piece_view: Option<PieceView>,
}

impl Tetris {
    /// Creates a new Tetris game with the specified dimensions
    pub fn new(width: usize, height: usize) -> Self {
        let console = Console::new();
        let board = Board::new(width, height);
        let board_view = BoardView::new(Console::new());
        
        Tetris {
            board,
            board_view,
            game_running: true,
            score: 0,
            console,
            current_piece_view: None,
        }
    }

    /// Spawns a new random piece
    pub fn spawn_new_piece(&mut self) {
        let piece_types = [PieceType::I, PieceType::O, PieceType::T, PieceType::L];
        let mut rng = rand::thread_rng();
        let random_index = rng.gen_range(0..piece_types.len());
        let piece_type = piece_types[random_index];
        
        let piece = Piece::new(piece_type);
        let piece_view = PieceView::from_piece_type(piece_type);
        
        self.board.set_current_piece(piece);
        self.current_piece_view = Some(piece_view);
    }

    /// Processes user input commands
    fn process_user_input(&mut self, input: &str) -> Result<(), io::Error> {
        if !input.is_empty() {
            if let Some(command) = input.chars().next() {
                match command {
                    '4' => {
                        // Move left
                        if let Some(piece) = self.board.current_piece() {
                            if self.board.can_move_piece(piece, -1, 0) {
                                if let Some(piece) = self.board.current_piece_mut() {
                                    piece.move_left();
                                }
                            }
                        }
                    }
                    '6' => {
                        // Move right
                        if let Some(piece) = self.board.current_piece() {
                            if self.board.can_move_piece(piece, 1, 0) {
                                if let Some(piece) = self.board.current_piece_mut() {
                                    piece.move_right();
                                }
                            }
                        }
                    }
                    '7' => {
                        // Rotate counter-clockwise
                        self.board.can_rotate_piece(false);
                    }
                    '9' => {
                        // Rotate clockwise
                        self.board.can_rotate_piece(true);
                    }
                    _ => {
                        // Invalid command, ignore
                    }
                }
            }
        }
        Ok(())
    }

    /// Main game loop
    pub fn game_loop(&mut self) -> Result<(), io::Error> {
        self.spawn_new_piece();
        
        while self.game_running {
            // Display the board
            self.board_view.display(&self.board, self.current_piece_view.as_ref())?;
            
            // Get user input
            let prompt = format!(
                "Score: {}\n\nComando (4=izq, 6=der, 7=rotar↺, 9=rotar↻): ",
                self.score
            );
            let input = self.console.read_string(&prompt)?;
            
            // Process user input
            self.process_user_input(&input)?;
            
            // Try to move piece down
            if let Some(current_piece) = self.board.current_piece() {
                if self.board.can_move_piece(current_piece, 0, 1) {
                    // Move piece down
                    if let Some(piece) = self.board.current_piece_mut() {
                        piece.move_down();
                    }
                } else {
                    // Piece can't move down, place it
                    let (_symbol, should_continue) = if let Some(current_piece) = self.board.current_piece() {
                        let symbol = self.current_piece_view
                            .as_ref()
                            .map(|v| v.symbol())
                            .unwrap_or('?');
                        
                        // Clone the piece to avoid borrowing issues
                        let piece_to_place = current_piece.clone();
                        self.board.place_piece(&piece_to_place, symbol);
                        
                        // Clear complete lines
                        let lines_cleared = self.board.clear_complete_lines();
                        self.score += lines_cleared * 100;
                        
                        if lines_cleared > 0 {
                            self.console.writeln_colored(
                                &format!("¡{lines_cleared} línea(s) eliminada(s)!"),
                                ForegroundColor::Green
                            )?;
                            self.console.read_string("Presiona Enter para continuar...")?;
                        }
                        
                        (symbol, true)
                    } else {
                        ('?', false)
                    };
                    
                    if should_continue {
                        // Spawn new piece
                        self.spawn_new_piece();
                        
                        // Check for game over
                        if let Some(new_piece) = self.board.current_piece() {
                            if !self.board.can_move_piece(new_piece, 0, 0) {
                                self.game_running = false;
                                self.console.writeln_colored("¡GAME OVER!", ForegroundColor::Red)?;
                                self.console.writeln(&format!("Puntuación final: {}", self.score))?;
                            }
                        }
                    }
                }
            }
        }
        
        Ok(())
    }

    /// Gets the current score
    #[allow(dead_code)]
    pub fn score(&self) -> usize {
        self.score
    }

    /// Checks if the game is running
    #[allow(dead_code)]
    pub fn is_running(&self) -> bool {
        self.game_running
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tetris_creation() {
        let tetris = Tetris::new(10, 20);
        assert_eq!(tetris.score(), 0);
        assert!(tetris.is_running());
    }

    #[test]
    fn test_spawn_new_piece() {
        let mut tetris = Tetris::new(10, 20);
        tetris.spawn_new_piece();
        
        assert!(tetris.board.current_piece().is_some());
        assert!(tetris.current_piece_view.is_some());
    }

    #[test]
    fn test_process_user_input() {
        let mut tetris = Tetris::new(10, 20);
        tetris.spawn_new_piece();
        
        // Test that processing input doesn't panic
        let result = tetris.process_user_input("4");
        assert!(result.is_ok());
        
        let result = tetris.process_user_input("6");
        assert!(result.is_ok());
        
        let result = tetris.process_user_input("7");
        assert!(result.is_ok());
        
        let result = tetris.process_user_input("9");
        assert!(result.is_ok());
    }
}