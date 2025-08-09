import { PieceFactory } from '../js-src/pieceFactory.js';

describe('PieceFactory', () => {
  test('should create I-piece correctly', () => {
    const piece = PieceFactory.createIPiece();
    const expectedShape = [[true, true, true, true]];
    expect(piece.getShape()).toEqual(expectedShape);
  });

  test('should create O-piece correctly', () => {
    const piece = PieceFactory.createOPiece();
    const expectedShape = [
      [true, true],
      [true, true]
    ];
    expect(piece.getShape()).toEqual(expectedShape);
  });

  test('should create T-piece correctly', () => {
    const piece = PieceFactory.createTPiece();
    const expectedShape = [
      [false, true, false],
      [true, true, true]
    ];
    expect(piece.getShape()).toEqual(expectedShape);
  });

  test('should create L-piece correctly', () => {
    const piece = PieceFactory.createLPiece();
    const expectedShape = [
      [true, false],
      [true, false],
      [true, true]
    ];
    expect(piece.getShape()).toEqual(expectedShape);
  });

  test('should create I-piece view with correct symbol', () => {
    const pieceView = PieceFactory.createIPieceView();
    expect(pieceView.getSymbol()).toBe('I');
  });

  test('should create O-piece view with correct symbol', () => {
    const pieceView = PieceFactory.createOPieceView();
    expect(pieceView.getSymbol()).toBe('O');
  });

  test('should create T-piece view with correct symbol', () => {
    const pieceView = PieceFactory.createTPieceView();
    expect(pieceView.getSymbol()).toBe('T');
  });

  test('should create L-piece view with correct symbol', () => {
    const pieceView = PieceFactory.createLPieceView();
    expect(pieceView.getSymbol()).toBe('L');
  });
});