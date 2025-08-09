#pragma once

#include "tetris/Piece.h"
#include <vector>

namespace tetris {

/**
 * @brief Handles rendering of Tetris pieces
 * 
 * Manages the visual representation of pieces on the game board.
 */
class PieceView {
public:
    /**
     * @brief Construct a new PieceView object
     * @param symbol The character symbol to represent this piece
     */
    explicit PieceView(char symbol);

    /**
     * @brief Get the piece symbol
     * @return char The symbol representing this piece
     */
    char getSymbol() const;

    /**
     * @brief Set the piece symbol
     * @param symbol The new symbol for this piece
     */
    void setSymbol(char symbol);

    /**
     * @brief Render the piece onto a display grid
     * @param piece The piece to render
     * @param displayGrid The grid to render onto
     */
    void render(const Piece& piece, std::vector<std::vector<char>>& displayGrid) const;

private:
    char symbol_;
};

} // namespace tetris