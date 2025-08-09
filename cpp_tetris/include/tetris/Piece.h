#pragma once

#include "tetris/Position.h"
#include <vector>

namespace tetris {

/**
 * @brief Represents a Tetris piece with its shape and position
 * 
 * Handles piece movement, rotation, and shape representation.
 * Uses a 2D boolean vector to represent the piece shape.
 */
class Piece {
public:
    /**
     * @brief Construct a new Piece object
     * @param shape 2D boolean vector representing the piece shape
     */
    explicit Piece(const std::vector<std::vector<bool>>& shape);

    /**
     * @brief Get the piece shape
     * @return const std::vector<std::vector<bool>>& Reference to the shape
     */
    const std::vector<std::vector<bool>>& getShape() const;

    /**
     * @brief Get the piece position
     * @return Position& Reference to the position
     */
    Position& getPosition();

    /**
     * @brief Get the piece position (const version)
     * @return const Position& Const reference to the position
     */
    const Position& getPosition() const;

    /**
     * @brief Move the piece down by one unit
     */
    void moveDown();

    /**
     * @brief Move the piece left by one unit
     */
    void moveLeft();

    /**
     * @brief Move the piece right by one unit
     */
    void moveRight();

    /**
     * @brief Rotate the piece clockwise
     */
    void rotateClockwise();

    /**
     * @brief Rotate the piece counter-clockwise
     */
    void rotateCounterClockwise();

    /**
     * @brief Rotate the piece (default clockwise)
     */
    void rotate();

private:
    std::vector<std::vector<bool>> shape_;
    Position position_;
};

} // namespace tetris