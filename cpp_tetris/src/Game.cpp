#include "tetris/Game.h"
#include "tetris/Tetris.h"

int main() {
    return tetris::Game::run();
}

namespace tetris {

int Game::run() {
    Tetris game(10, 20);
    game.gameLoop();
    return 0;
}

} // namespace tetris