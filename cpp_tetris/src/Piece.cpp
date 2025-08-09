#include "tetris/Piece.h"

namespace tetris {

Piece::Piece(const std::vector<std::vector<bool>>& shape)
    : shape_(shape), position_(0, 0) {}

const std::vector<std::vector<bool>>& Piece::getShape() const {
    return shape_;
}

Position& Piece::getPosition() {
    return position_;
}

const Position& Piece::getPosition() const {
    return position_;
}

void Piece::moveDown() {
    position_.setY(position_.getY() + 1);
}

void Piece::moveLeft() {
    position_.setX(position_.getX() - 1);
}

void Piece::moveRight() {
    position_.setX(position_.getX() + 1);
}

void Piece::rotateClockwise() {
    const int rows = static_cast<int>(shape_.size());
    const int cols = static_cast<int>(shape_[0].size());
    
    std::vector<std::vector<bool>> rotated(cols, std::vector<bool>(rows));
    
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            rotated[j][rows - 1 - i] = shape_[i][j];
        }
    }
    
    shape_ = std::move(rotated);
}

void Piece::rotateCounterClockwise() {
    const int rows = static_cast<int>(shape_.size());
    const int cols = static_cast<int>(shape_[0].size());
    
    std::vector<std::vector<bool>> rotated(cols, std::vector<bool>(rows));
    
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            rotated[cols - 1 - j][i] = shape_[i][j];
        }
    }
    
    shape_ = std::move(rotated);
}

void Piece::rotate() {
    rotateClockwise();
}

} // namespace tetris