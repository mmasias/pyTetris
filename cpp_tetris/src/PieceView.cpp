#include "tetris/PieceView.h"

namespace tetris {

PieceView::PieceView(char symbol) : symbol_(symbol) {}

char PieceView::getSymbol() const {
    return symbol_;
}

void PieceView::setSymbol(char symbol) {
    symbol_ = symbol;
}

void PieceView::render(const Piece& piece, std::vector<std::vector<char>>& displayGrid) const {
    const auto& shape = piece.getShape();
    const auto& pos = piece.getPosition();

    for (int i = 0; i < static_cast<int>(shape.size()); ++i) {
        for (int j = 0; j < static_cast<int>(shape[i].size()); ++j) {
            if (shape[i][j]) {
                int x = pos.getX() + j;
                int y = pos.getY() + i;
                
                if (y >= 0 && y < static_cast<int>(displayGrid.size()) && 
                    x >= 0 && x < static_cast<int>(displayGrid[0].size())) {
                    displayGrid[y][x] = symbol_;
                }
            }
        }
    }
}

} // namespace tetris