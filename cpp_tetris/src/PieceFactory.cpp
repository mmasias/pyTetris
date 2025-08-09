#include "tetris/PieceFactory.h"

namespace tetris {

std::shared_ptr<Piece> PieceFactory::createIPiece() {
    std::vector<std::vector<bool>> shape = {
        { true, true, true, true }
    };
    return std::make_shared<Piece>(shape);
}

std::shared_ptr<Piece> PieceFactory::createOPiece() {
    std::vector<std::vector<bool>> shape = {
        { true, true },
        { true, true }
    };
    return std::make_shared<Piece>(shape);
}

std::shared_ptr<Piece> PieceFactory::createTPiece() {
    std::vector<std::vector<bool>> shape = {
        { false, true, false },
        { true,  true, true  }
    };
    return std::make_shared<Piece>(shape);
}

std::shared_ptr<Piece> PieceFactory::createLPiece() {
    std::vector<std::vector<bool>> shape = {
        { true, false },
        { true, false },
        { true, true  }
    };
    return std::make_shared<Piece>(shape);
}

std::unique_ptr<PieceView> PieceFactory::createIPieceView() {
    return std::make_unique<PieceView>('I');
}

std::unique_ptr<PieceView> PieceFactory::createOPieceView() {
    return std::make_unique<PieceView>('O');
}

std::unique_ptr<PieceView> PieceFactory::createTPieceView() {
    return std::make_unique<PieceView>('T');
}

std::unique_ptr<PieceView> PieceFactory::createLPieceView() {
    return std::make_unique<PieceView>('L');
}

} // namespace tetris