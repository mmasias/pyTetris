#pragma once

#include "tetris/Board.h"
#include "tetris/PieceView.h"
#include "tetris/Console.h"
#include <memory>

namespace tetris {

/**
 * @brief Handles rendering of the game board
 * 
 * Manages the visual display of the complete Tetris game board
 * including placed pieces and the current falling piece.
 */
class BoardView {
public:
    /**
     * @brief Construct a new BoardView object
     * @param console Console object for output
     */
    explicit BoardView(std::shared_ptr<Console> console);

    /**
     * @brief Display the board with current piece
     * @param board The game board to display
     * @param currentPieceView View for the current piece (can be nullptr)
     */
    void display(const Board& board, const PieceView* currentPieceView = nullptr) const;

private:
    std::shared_ptr<Console> console_;
};

} // namespace tetris