/**
 * Handles the visual representation of a Tetris piece.
 * Converted from Java PieceView class.
 */
export class PieceView {
  /**
   * Creates a new PieceView instance.
   * @param {string} symbol - The character symbol to represent this piece
   */
  constructor(symbol) {
    this.symbol = symbol;
  }

  /**
   * Gets the piece symbol.
   * @returns {string} The piece symbol
   */
  getSymbol() {
    return this.symbol;
  }

  /**
   * Sets the piece symbol.
   * @param {string} symbol - The new piece symbol
   */
  setSymbol(symbol) {
    this.symbol = symbol;
  }

  /**
   * Renders the piece onto the display grid.
   * @param {Piece} piece - The piece to render
   * @param {string[][]} displayGrid - The grid to render onto
   */
  render(piece, displayGrid) {
    const shape = piece.getShape();
    const pos = piece.getPosition();

    for (let i = 0; i < shape.length; i++) {
      for (let j = 0; j < shape[i].length; j++) {
        if (shape[i][j]) {
          const x = pos.getX() + j;
          const y = pos.getY() + i;
          if (y >= 0 && y < displayGrid.length && x >= 0 && x < displayGrid[0].length) {
            displayGrid[y][x] = this.symbol;
          }
        }
      }
    }
  }
}