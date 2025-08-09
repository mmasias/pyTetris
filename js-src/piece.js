import { Position } from './position.js';

/**
 * Represents a Tetris piece with shape and position.
 * Converted from Java Piece class.
 */
export class Piece {
  /**
   * Creates a new Piece instance.
   * @param {boolean[][]} shape - 2D array representing the piece shape
   */
  constructor(shape) {
    this.shape = shape;
    this.position = new Position(0, 0);
  }

  /**
   * Gets the piece shape.
   * @returns {boolean[][]} The piece shape
   */
  getShape() {
    return this.shape;
  }

  /**
   * Gets the piece position.
   * @returns {Position} The piece position
   */
  getPosition() {
    return this.position;
  }

  /**
   * Moves the piece down by one unit.
   */
  moveDown() {
    this.position.setY(this.position.getY() + 1);
  }

  /**
   * Moves the piece left by one unit.
   */
  moveLeft() {
    this.position.setX(this.position.getX() - 1);
  }

  /**
   * Moves the piece right by one unit.
   */
  moveRight() {
    this.position.setX(this.position.getX() + 1);
  }

  /**
   * Rotates the piece (alias for rotateClockwise).
   */
  rotate() {
    this.rotateClockwise();
  }

  /**
   * Rotates the piece 90 degrees clockwise.
   */
  rotateClockwise() {
    const rows = this.shape.length;
    const cols = this.shape[0].length;
    const rotated = Array(cols).fill(null).map(() => Array(rows).fill(false));

    for (let i = 0; i < rows; i++) {
      for (let j = 0; j < cols; j++) {
        rotated[j][rows - 1 - i] = this.shape[i][j];
      }
    }
    this.shape = rotated;
  }

  /**
   * Rotates the piece 90 degrees counterclockwise.
   */
  rotateCounterClockwise() {
    const rows = this.shape.length;
    const cols = this.shape[0].length;
    const rotated = Array(cols).fill(null).map(() => Array(rows).fill(false));

    for (let i = 0; i < rows; i++) {
      for (let j = 0; j < cols; j++) {
        rotated[cols - 1 - j][i] = this.shape[i][j];
      }
    }
    this.shape = rotated;
  }
}