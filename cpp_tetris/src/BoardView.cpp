#include "tetris/BoardView.h"
#include <algorithm>

namespace tetris {

BoardView::BoardView(std::shared_ptr<Console> console) : console_(console) {}

void BoardView::display(const Board& board, const PieceView* currentPieceView) const {
    const int width = board.getWidth();
    const int height = board.getHeight();
    const auto& grid = board.getGrid();
    
    // Create display grid by copying the board grid
    std::vector<std::vector<char>> displayGrid(height, std::vector<char>(width));
    for (int i = 0; i < height; ++i) {
        for (int j = 0; j < width; ++j) {
            displayGrid[i][j] = grid[i][j];
        }
    }

    // Render current piece if it exists
    if (currentPieceView != nullptr && board.getCurrentPiece() != nullptr) {
        currentPieceView->render(*board.getCurrentPiece(), displayGrid);
    }

    // Clear screen and display the board
    console_->clearScreen();
    
    // Top border
    console_->write("<!");
    for (int i = 0; i < ((width + 2) * 2) - 1; ++i) {
        console_->write("=");
    }
    console_->writeln("!>");
    
    // Board content
    for (int i = 0; i < height; ++i) {
        console_->write("<!  ");
        for (int j = 0; j < width; ++j) {
            console_->write(std::string(1, displayGrid[i][j]) + " ");
        }
        console_->writeln(" !>");
    }
    
    // Bottom border
    console_->write("<!");
    for (int i = 0; i < ((width + 2) * 2) - 1; ++i) {
        console_->write("=");
    }
    console_->writeln("!>");
    
    // Bottom decoration
    for (int i = 0; i < width + 4; ++i) {
        console_->write("\\/");
    }
    console_->writeln("");
}

} // namespace tetris