#include "tetris/Tetris.h"
#include <vector>
#include <memory>
#include <functional>

namespace tetris {

Tetris::Tetris(int width, int height) 
    : gameRunning_(true), score_(0), gen_(rd_()), dis_(0, 3) {
    console_ = std::make_shared<Console>();
    board_ = std::make_unique<Board>(width, height);
    boardView_ = std::make_shared<BoardView>(console_);
}

void Tetris::spawnNewPiece() {
    // Array of piece creation functions
    std::vector<std::function<std::shared_ptr<Piece>()>> pieceCreators = {
        PieceFactory::createIPiece,
        PieceFactory::createOPiece,
        PieceFactory::createTPiece,
        PieceFactory::createLPiece
    };

    std::vector<std::function<std::unique_ptr<PieceView>()>> pieceViewCreators = {
        PieceFactory::createIPieceView,
        PieceFactory::createOPieceView,
        PieceFactory::createTPieceView,
        PieceFactory::createLPieceView
    };

    int randomIndex = dis_(gen_);
    board_->setCurrentPiece(pieceCreators[randomIndex]());
    currentPieceView_ = pieceViewCreators[randomIndex]();
}

void Tetris::processUserInput(const std::string& input) {
    if (!input.empty()) {
        char command = input[0];
        auto currentPiece = board_->getCurrentPiece();
        
        if (!currentPiece) return;

        switch (command) {
            case '4': // Move left
                if (board_->canMovePiece(*currentPiece, -1, 0)) {
                    currentPiece->moveLeft();
                }
                break;
            case '6': // Move right
                if (board_->canMovePiece(*currentPiece, 1, 0)) {
                    currentPiece->moveRight();
                }
                break;
            case '7': // Rotate counter-clockwise
                board_->canRotatePiece(*currentPiece, false);
                break;
            case '9': // Rotate clockwise
                board_->canRotatePiece(*currentPiece, true);
                break;
        }
    }
}

void Tetris::gameLoop() {
    spawnNewPiece();
    
    while (gameRunning_) {
        boardView_->display(*board_, currentPieceView_.get());
        
        std::string prompt = "Score: " + std::to_string(score_) + 
                           "\n\nComando (4=izq, 6=der, 7=rotar↺, 9=rotar↻): ";
        std::string input = console_->readString(prompt);
        
        processUserInput(input);

        auto currentPiece = board_->getCurrentPiece();
        if (!currentPiece) {
            continue;
        }

        // Try to move piece down
        if (board_->canMovePiece(*currentPiece, 0, 1)) {
            currentPiece->moveDown();
        } else {
            // Piece can't move down, place it
            board_->placePiece(*currentPiece, currentPieceView_->getSymbol());

            // Clear complete lines
            int linesCleared = board_->clearCompleteLines();
            score_ += linesCleared * 100;

            if (linesCleared > 0) {
                console_->writeln("¡" + std::to_string(linesCleared) + " línea(s) eliminada(s)!");
                console_->readString("Presiona Enter para continuar...");
            }

            // Spawn new piece
            spawnNewPiece();

            // Check game over
            if (!board_->canMovePiece(*board_->getCurrentPiece(), 0, 0)) {
                gameRunning_ = false;
                console_->writeln("¡GAME OVER!");
                console_->writeln("Puntuación final: " + std::to_string(score_));
            }
        }
    }
}

} // namespace tetris