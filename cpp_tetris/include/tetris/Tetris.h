#pragma once

#include "tetris/Board.h"
#include "tetris/BoardView.h"
#include "tetris/Console.h"
#include "tetris/PieceView.h"
#include "tetris/PieceFactory.h"
#include <memory>
#include <random>

namespace tetris {

/**
 * @brief Main Tetris game class
 * 
 * Manages the game state, user input processing, and game loop.
 */
class Tetris {
public:
    /**
     * @brief Construct a new Tetris game
     * @param width Board width
     * @param height Board height
     */
    Tetris(int width, int height);

    /**
     * @brief Start the game loop
     */
    void gameLoop();

private:
    /**
     * @brief Spawn a new random piece
     */
    void spawnNewPiece();

    /**
     * @brief Process user input commands
     * @param input User input string
     */
    void processUserInput(const std::string& input);

    std::unique_ptr<Board> board_;
    std::shared_ptr<BoardView> boardView_;
    std::shared_ptr<Console> console_;
    std::unique_ptr<PieceView> currentPieceView_;
    
    bool gameRunning_;
    int score_;
    
    // Random number generation
    std::random_device rd_;
    std::mt19937 gen_;
    std::uniform_int_distribution<> dis_;
};

} // namespace tetris