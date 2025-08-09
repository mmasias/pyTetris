#include "tetris/Position.h"

namespace tetris {

Position::Position(int x, int y) : x_(x), y_(y) {}

int Position::getX() const {
    return x_;
}

int Position::getY() const {
    return y_;
}

void Position::setX(int x) {
    x_ = x;
}

void Position::setY(int y) {
    y_ = y;
}

Position Position::add(const Position& other) const {
    return Position(x_ + other.x_, y_ + other.y_);
}

bool Position::operator==(const Position& other) const {
    return x_ == other.x_ && y_ == other.y_;
}

bool Position::operator!=(const Position& other) const {
    return !(*this == other);
}

} // namespace tetris