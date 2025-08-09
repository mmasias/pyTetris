#pragma once

#include "tetris/Piece.h"
#include <vector>
#include <memory>

namespace tetris {

/**
 * @brief Represents the Tetris game board
 * 
 * Manages the game grid, piece placement, collision detection,
 * and line clearing functionality.
 */
class Board {
public:
    /**
     * @brief Construct a new Board object
     * @param width Width of the board
     * @param height Height of the board
     */
    Board(int width, int height);

    /**
     * @brief Get the board width
     * @return int The width of the board
     */
    int getWidth() const;

    /**
     * @brief Get the board height
     * @return int The height of the board
     */
    int getHeight() const;

    /**
     * @brief Get the game grid
     * @return const std::vector<std::vector<char>>& Reference to the grid
     */
    const std::vector<std::vector<char>>& getGrid() const;

    /**
     * @brief Set the current piece
     * @param piece Shared pointer to the piece to set as current
     */
    void setCurrentPiece(std::shared_ptr<Piece> piece);

    /**
     * @brief Get the current piece
     * @return std::shared_ptr<Piece> Shared pointer to the current piece
     */
    std::shared_ptr<Piece> getCurrentPiece() const;

    /**
     * @brief Check if a piece can move by the given offset
     * @param piece The piece to check
     * @param deltaX X offset to check
     * @param deltaY Y offset to check
     * @return true if the piece can move, false otherwise
     */
    bool canMovePiece(const Piece& piece, int deltaX, int deltaY) const;

    /**
     * @brief Check if a piece can rotate
     * @param piece The piece to check rotation for
     * @param clockwise True for clockwise, false for counter-clockwise
     * @return true if the piece can rotate, false otherwise
     */
    bool canRotatePiece(Piece& piece, bool clockwise);

    /**
     * @brief Place a piece on the board
     * @param piece The piece to place
     * @param symbol The character symbol to use for the piece
     */
    void placePiece(const Piece& piece, char symbol);

    /**
     * @brief Clear complete lines from the board
     * @return int Number of lines cleared
     */
    int clearCompleteLines();

private:
    /**
     * @brief Clear the board by filling with empty characters
     */
    void clearBoard();

    std::vector<std::vector<char>> grid_;
    int width_;
    int height_;
    std::shared_ptr<Piece> currentPiece_;
    static constexpr char EMPTY_CELL = '.';
};

} // namespace tetris