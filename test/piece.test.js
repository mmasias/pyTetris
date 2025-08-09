import { Piece } from '../js-src/piece.js';

describe('Piece', () => {
  test('should create a piece with a shape', () => {
    const shape = [[true, true], [true, false]];
    const piece = new Piece(shape);
    
    expect(piece.getShape()).toEqual(shape);
    expect(piece.getPosition().getX()).toBe(0);
    expect(piece.getPosition().getY()).toBe(0);
  });

  test('should move down correctly', () => {
    const piece = new Piece([[true]]);
    piece.moveDown();
    expect(piece.getPosition().getY()).toBe(1);
  });

  test('should move left correctly', () => {
    const piece = new Piece([[true]]);
    piece.moveLeft();
    expect(piece.getPosition().getX()).toBe(-1);
  });

  test('should move right correctly', () => {
    const piece = new Piece([[true]]);
    piece.moveRight();
    expect(piece.getPosition().getX()).toBe(1);
  });

  test('should rotate clockwise correctly', () => {
    const shape = [
      [true, false],
      [true, true]
    ];
    const piece = new Piece(shape);
    piece.rotateClockwise();
    
    const expected = [
      [true, true],
      [true, false]
    ];
    expect(piece.getShape()).toEqual(expected);
  });

  test('should rotate counter-clockwise correctly', () => {
    const shape = [
      [true, false],
      [true, true]
    ];
    const piece = new Piece(shape);
    piece.rotateCounterClockwise();
    
    const expected = [
      [false, true],
      [true, true]
    ];
    expect(piece.getShape()).toEqual(expected);
  });

  test('should handle L-piece rotation', () => {
    const shape = [
      [true, false],
      [true, false],
      [true, true]
    ];
    const piece = new Piece(shape);
    piece.rotateClockwise();
    
    const expected = [
      [true, true, true],
      [true, false, false]
    ];
    expect(piece.getShape()).toEqual(expected);
  });
});