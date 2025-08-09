#pragma once

#include "tetris/Piece.h"
#include "tetris/PieceView.h"
#include <memory>

namespace tetris {

/**
 * @brief Factory class for creating Tetris pieces and their views
 * 
 * Implements the Factory pattern to create different types of Tetris pieces
 * along with their corresponding visual representations.
 */
class PieceFactory {
public:
    /**
     * @brief Create an I-piece (straight line)
     * @return std::shared_ptr<Piece> Pointer to the created I-piece
     */
    static std::shared_ptr<Piece> createIPiece();

    /**
     * @brief Create an O-piece (square)
     * @return std::shared_ptr<Piece> Pointer to the created O-piece
     */
    static std::shared_ptr<Piece> createOPiece();

    /**
     * @brief Create a T-piece (T-shape)
     * @return std::shared_ptr<Piece> Pointer to the created T-piece
     */
    static std::shared_ptr<Piece> createTPiece();

    /**
     * @brief Create an L-piece (L-shape)
     * @return std::shared_ptr<Piece> Pointer to the created L-piece
     */
    static std::shared_ptr<Piece> createLPiece();

    /**
     * @brief Create a view for an I-piece
     * @return std::unique_ptr<PieceView> Pointer to the created I-piece view
     */
    static std::unique_ptr<PieceView> createIPieceView();

    /**
     * @brief Create a view for an O-piece
     * @return std::unique_ptr<PieceView> Pointer to the created O-piece view
     */
    static std::unique_ptr<PieceView> createOPieceView();

    /**
     * @brief Create a view for a T-piece
     * @return std::unique_ptr<PieceView> Pointer to the created T-piece view
     */
    static std::unique_ptr<PieceView> createTPieceView();

    /**
     * @brief Create a view for an L-piece
     * @return std::unique_ptr<PieceView> Pointer to the created L-piece view
     */
    static std::unique_ptr<PieceView> createLPieceView();
};

} // namespace tetris