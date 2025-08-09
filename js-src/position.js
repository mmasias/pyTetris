/**
 * Represents a 2D position with x and y coordinates.
 * Converted from Java Position class.
 */
export class Position {
  /**
   * Creates a new Position instance.
   * @param {number} x - The x coordinate
   * @param {number} y - The y coordinate
   */
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  /**
   * Gets the x coordinate.
   * @returns {number} The x coordinate
   */
  getX() {
    return this.x;
  }

  /**
   * Gets the y coordinate.
   * @returns {number} The y coordinate
   */
  getY() {
    return this.y;
  }

  /**
   * Sets the x coordinate.
   * @param {number} x - The new x coordinate
   */
  setX(x) {
    this.x = x;
  }

  /**
   * Sets the y coordinate.
   * @param {number} y - The new y coordinate
   */
  setY(y) {
    this.y = y;
  }

  /**
   * Adds another position to this position and returns a new Position.
   * @param {Position} other - The position to add
   * @returns {Position} A new Position with the sum of coordinates
   */
  add(other) {
    return new Position(this.x + other.x, this.y + other.y);
  }
}