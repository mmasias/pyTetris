#pragma once

namespace tetris {

/**
 * @brief Main game entry point
 * 
 * Simple class that serves as the entry point for the Tetris game.
 */
class Game {
public:
    /**
     * @brief Run the Tetris game
     * @return int Exit code (0 for success)
     */
    static int run();
};

} // namespace tetris