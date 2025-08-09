import { Piece } from './piece.js';
import { PieceView } from './pieceView.js';

/**
 * Factory class for creating different types of Tetris pieces.
 * Converted from Java PieceFactory class.
 */
export class PieceFactory {
  /**
   * Creates an I-piece (straight line).
   * @returns {Piece} The I-piece
   */
  static createIPiece() {
    const shape = [
      [true, true, true, true]
    ];
    return new Piece(shape);
  }

  /**
   * Creates an O-piece (square).
   * @returns {Piece} The O-piece
   */
  static createOPiece() {
    const shape = [
      [true, true],
      [true, true]
    ];
    return new Piece(shape);
  }

  /**
   * Creates a T-piece.
   * @returns {Piece} The T-piece
   */
  static createTPiece() {
    const shape = [
      [false, true, false],
      [true, true, true]
    ];
    return new Piece(shape);
  }

  /**
   * Creates an L-piece.
   * @returns {Piece} The L-piece
   */
  static createLPiece() {
    const shape = [
      [true, false],
      [true, false],
      [true, true]
    ];
    return new Piece(shape);
  }

  /**
   * Creates a view for an I-piece.
   * @returns {PieceView} The I-piece view
   */
  static createIPieceView() {
    return new PieceView('I');
  }

  /**
   * Creates a view for an O-piece.
   * @returns {PieceView} The O-piece view
   */
  static createOPieceView() {
    return new PieceView('O');
  }

  /**
   * Creates a view for a T-piece.
   * @returns {PieceView} The T-piece view
   */
  static createTPieceView() {
    return new PieceView('T');
  }

  /**
   * Creates a view for an L-piece.
   * @returns {PieceView} The L-piece view
   */
  static createLPieceView() {
    return new PieceView('L');
  }
}