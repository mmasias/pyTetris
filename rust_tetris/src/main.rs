mod position;
mod piece;
mod board;
mod console;
mod piece_view;
mod board_view;
mod tetris;

use tetris::Tetris;
use std::io;

fn main() -> Result<(), io::Error> {
    let mut game = Tetris::new(10, 20);
    game.game_loop()
}
