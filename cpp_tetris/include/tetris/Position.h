#pragma once

namespace tetris {

/**
 * @brief Represents a 2D position with x and y coordinates
 * 
 * Simple utility class for handling 2D coordinates in the Tetris game.
 * Used for piece positioning and movement calculations.
 */
class Position {
public:
    /**
     * @brief Construct a new Position object
     * @param x The x coordinate
     * @param y The y coordinate
     */
    Position(int x = 0, int y = 0);

    /**
     * @brief Get the x coordinate
     * @return int The x coordinate
     */
    int getX() const;

    /**
     * @brief Get the y coordinate
     * @return int The y coordinate
     */
    int getY() const;

    /**
     * @brief Set the x coordinate
     * @param x The new x coordinate
     */
    void setX(int x);

    /**
     * @brief Set the y coordinate
     * @param y The new y coordinate
     */
    void setY(int y);

    /**
     * @brief Add two positions together
     * @param other The other position to add
     * @return Position A new position representing the sum
     */
    Position add(const Position& other) const;

    /**
     * @brief Equality operator
     * @param other The other position to compare with
     * @return true if positions are equal, false otherwise
     */
    bool operator==(const Position& other) const;

    /**
     * @brief Inequality operator
     * @param other The other position to compare with
     * @return true if positions are not equal, false otherwise
     */
    bool operator!=(const Position& other) const;

private:
    int x_;
    int y_;
};

} // namespace tetris