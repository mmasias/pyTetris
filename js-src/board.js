/**
 * Represents the Tetris game board with grid management and piece collision detection.
 * Converted from Java Board class.
 */
export class Board {
  /**
   * Creates a new Board instance.
   * @param {number} width - The board width
   * @param {number} height - The board height
   */
  constructor(width, height) {
    this.width = width;
    this.height = height;
    this.grid = Array(height).fill(null).map(() => Array(width).fill('.'));
    this.currentPiece = null;
    this.clearBoard();
  }

  /**
   * Clears the board by setting all cells to empty ('.').
   */
  clearBoard() {
    for (let i = 0; i < this.height; i++) {
      for (let j = 0; j < this.width; j++) {
        this.grid[i][j] = '.';
      }
    }
  }

  /**
   * Gets the board width.
   * @returns {number} The board width
   */
  getWidth() {
    return this.width;
  }

  /**
   * Gets the board height.
   * @returns {number} The board height
   */
  getHeight() {
    return this.height;
  }

  /**
   * Gets the board grid.
   * @returns {string[][]} The board grid
   */
  getGrid() {
    return this.grid;
  }

  /**
   * Sets the current piece and positions it at the top center.
   * @param {Piece} piece - The piece to set as current
   */
  setCurrentPiece(piece) {
    this.currentPiece = piece;
    piece.getPosition().setX(Math.floor(this.width / 2) - Math.floor(piece.getShape()[0].length / 2));
    piece.getPosition().setY(0);
  }

  /**
   * Gets the current piece.
   * @returns {Piece|null} The current piece
   */
  getCurrentPiece() {
    return this.currentPiece;
  }

  /**
   * Checks if a piece can move to a new position.
   * @param {Piece} piece - The piece to check
   * @param {number} deltaX - The horizontal movement delta
   * @param {number} deltaY - The vertical movement delta
   * @returns {boolean} True if the move is valid
   */
  canMovePiece(piece, deltaX, deltaY) {
    const shape = piece.getShape();
    const pos = piece.getPosition();

    for (let i = 0; i < shape.length; i++) {
      for (let j = 0; j < shape[i].length; j++) {
        if (shape[i][j]) {
          const newX = pos.getX() + j + deltaX;
          const newY = pos.getY() + i + deltaY;

          // Check boundaries
          if (newX < 0 || newX >= this.width || newY >= this.height) {
            return false;
          }

          // Check collision with placed pieces
          if (newY >= 0 && this.grid[newY][newX] !== '.') {
            return false;
          }
        }
      }
    }
    return true;
  }

  /**
   * Attempts to rotate a piece and validates the rotation.
   * @param {Piece} piece - The piece to rotate
   * @param {boolean} clockwise - True for clockwise rotation
   * @returns {boolean} True if rotation was successful
   */
  canRotatePiece(piece, clockwise) {
    // Perform rotation
    if (clockwise) {
      piece.rotateClockwise();
    } else {
      piece.rotateCounterClockwise();
    }

    // Check if rotation is valid
    const canRotate = this.canMovePiece(piece, 0, 0);

    // If rotation is invalid, revert it
    if (!canRotate) {
      if (clockwise) {
        // Revert clockwise rotation with 3 counter-clockwise rotations
        piece.rotateCounterClockwise();
        piece.rotateCounterClockwise();
        piece.rotateCounterClockwise();
      } else {
        // Revert counter-clockwise rotation with 3 clockwise rotations
        piece.rotateClockwise();
        piece.rotateClockwise();
        piece.rotateClockwise();
      }
    }

    return canRotate;
  }

  /**
   * Places a piece permanently on the board.
   * @param {Piece} piece - The piece to place
   * @param {string} symbol - The symbol to use for the piece
   */
  placePiece(piece, symbol) {
    const shape = piece.getShape();
    const pos = piece.getPosition();

    for (let i = 0; i < shape.length; i++) {
      for (let j = 0; j < shape[i].length; j++) {
        if (shape[i][j]) {
          const x = pos.getX() + j;
          const y = pos.getY() + i;
          if (y >= 0 && y < this.height && x >= 0 && x < this.width) {
            this.grid[y][x] = symbol;
          }
        }
      }
    }
  }

  /**
   * Clears complete lines and returns the number of lines cleared.
   * @returns {number} The number of lines cleared
   */
  clearCompleteLines() {
    let linesCleared = 0;

    for (let i = this.height - 1; i >= 0; i--) {
      let isComplete = true;

      // Check if line is complete
      for (let j = 0; j < this.width && isComplete; j++) {
        if (this.grid[i][j] === '.') {
          isComplete = false;
        }
      }

      if (isComplete) {
        // Move all lines above down by one
        for (let k = i; k > 0; k--) {
          for (let j = 0; j < this.width; j++) {
            this.grid[k][j] = this.grid[k - 1][j];
          }
        }

        // Clear the top line
        for (let j = 0; j < this.width; j++) {
          this.grid[0][j] = '.';
        }

        linesCleared++;
        i++; // Check the same line again since we moved lines down
      }
    }

    return linesCleared;
  }
}