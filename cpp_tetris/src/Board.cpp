#include "tetris/Board.h"

namespace tetris {

Board::Board(int width, int height) 
    : width_(width), height_(height), currentPiece_(nullptr) {
    grid_.resize(height_, std::vector<char>(width_));
    clearBoard();
}

int Board::getWidth() const {
    return width_;
}

int Board::getHeight() const {
    return height_;
}

const std::vector<std::vector<char>>& Board::getGrid() const {
    return grid_;
}

void Board::setCurrentPiece(std::shared_ptr<Piece> piece) {
    currentPiece_ = piece;
    if (piece) {
        const auto& shape = piece->getShape();
        piece->getPosition().setX(width_ / 2 - static_cast<int>(shape[0].size()) / 2);
        piece->getPosition().setY(0);
    }
}

std::shared_ptr<Piece> Board::getCurrentPiece() const {
    return currentPiece_;
}

bool Board::canMovePiece(const Piece& piece, int deltaX, int deltaY) const {
    const auto& shape = piece.getShape();
    const auto& pos = piece.getPosition();

    for (int i = 0; i < static_cast<int>(shape.size()); ++i) {
        for (int j = 0; j < static_cast<int>(shape[i].size()); ++j) {
            if (shape[i][j]) {
                int newX = pos.getX() + j + deltaX;
                int newY = pos.getY() + i + deltaY;

                // Check boundaries
                if (newX < 0 || newX >= width_ || newY >= height_) {
                    return false;
                }

                // Check collision with existing pieces
                if (newY >= 0 && grid_[newY][newX] != EMPTY_CELL) {
                    return false;
                }
            }
        }
    }
    return true;
}

bool Board::canRotatePiece(Piece& piece, bool clockwise) {
    // Perform the rotation
    if (clockwise) {
        piece.rotateClockwise();
    } else {
        piece.rotateCounterClockwise();
    }

    // Check if the rotation is valid
    bool canRotate = canMovePiece(piece, 0, 0);

    // If rotation is invalid, revert it
    if (!canRotate) {
        if (clockwise) {
            // Revert clockwise rotation by rotating counter-clockwise 3 times
            piece.rotateCounterClockwise();
            piece.rotateCounterClockwise();
            piece.rotateCounterClockwise();
        } else {
            // Revert counter-clockwise rotation by rotating clockwise 3 times
            piece.rotateClockwise();
            piece.rotateClockwise();
            piece.rotateClockwise();
        }
    }

    return canRotate;
}

void Board::placePiece(const Piece& piece, char symbol) {
    const auto& shape = piece.getShape();
    const auto& pos = piece.getPosition();

    for (int i = 0; i < static_cast<int>(shape.size()); ++i) {
        for (int j = 0; j < static_cast<int>(shape[i].size()); ++j) {
            if (shape[i][j]) {
                int x = pos.getX() + j;
                int y = pos.getY() + i;
                if (y >= 0 && y < height_ && x >= 0 && x < width_) {
                    grid_[y][x] = symbol;
                }
            }
        }
    }
}

int Board::clearCompleteLines() {
    int linesCleared = 0;

    for (int i = height_ - 1; i >= 0; --i) {
        bool isComplete = true;

        // Check if line is complete
        for (int j = 0; j < width_ && isComplete; ++j) {
            if (grid_[i][j] == EMPTY_CELL) {
                isComplete = false;
            }
        }

        if (isComplete) {
            // Move all lines above down by one
            for (int k = i; k > 0; --k) {
                for (int j = 0; j < width_; ++j) {
                    grid_[k][j] = grid_[k - 1][j];
                }
            }

            // Clear the top line
            for (int j = 0; j < width_; ++j) {
                grid_[0][j] = EMPTY_CELL;
            }

            linesCleared++;
            i++; // Check the same line again since lines moved down
        }
    }

    return linesCleared;
}

void Board::clearBoard() {
    for (int i = 0; i < height_; ++i) {
        for (int j = 0; j < width_; ++j) {
            grid_[i][j] = EMPTY_CELL;
        }
    }
}

} // namespace tetris