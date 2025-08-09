import { Board } from '../js-src/board.js';
import { Piece } from '../js-src/piece.js';

describe('Board', () => {
  let board;

  beforeEach(() => {
    board = new Board(10, 20);
  });

  test('should create a board with correct dimensions', () => {
    expect(board.getWidth()).toBe(10);
    expect(board.getHeight()).toBe(20);
  });

  test('should initialize with empty grid', () => {
    const grid = board.getGrid();
    expect(grid.length).toBe(20);
    expect(grid[0].length).toBe(10);
    
    // Check all cells are empty
    for (let i = 0; i < 20; i++) {
      for (let j = 0; j < 10; j++) {
        expect(grid[i][j]).toBe('.');
      }
    }
  });

  test('should set current piece at center top', () => {
    const shape = [[true, true]];
    const piece = new Piece(shape);
    board.setCurrentPiece(piece);
    
    expect(board.getCurrentPiece()).toBe(piece);
    expect(piece.getPosition().getX()).toBe(4); // (10/2) - (2/2) = 4
    expect(piece.getPosition().getY()).toBe(0);
  });

  test('should detect valid piece movement', () => {
    const shape = [[true]];
    const piece = new Piece(shape);
    board.setCurrentPiece(piece);
    
    // Should be able to move right
    expect(board.canMovePiece(piece, 1, 0)).toBe(true);
    // Should be able to move down
    expect(board.canMovePiece(piece, 0, 1)).toBe(true);
  });

  test('should detect invalid piece movement at boundaries', () => {
    const shape = [[true]];
    const piece = new Piece(shape);
    piece.getPosition().setX(0);
    piece.getPosition().setY(0);
    
    // Should not be able to move left from x=0
    expect(board.canMovePiece(piece, -1, 0)).toBe(false);
    
    piece.getPosition().setX(9);
    // Should not be able to move right from x=9
    expect(board.canMovePiece(piece, 1, 0)).toBe(false);
    
    piece.getPosition().setY(19);
    // Should not be able to move down from y=19
    expect(board.canMovePiece(piece, 0, 1)).toBe(false);
  });

  test('should place piece on board', () => {
    const shape = [[true, true]];
    const piece = new Piece(shape);
    piece.getPosition().setX(0);
    piece.getPosition().setY(0);
    
    board.placePiece(piece, 'X');
    
    const grid = board.getGrid();
    expect(grid[0][0]).toBe('X');
    expect(grid[0][1]).toBe('X');
  });

  test('should clear complete lines', () => {
    const grid = board.getGrid();
    
    // Fill bottom line completely
    for (let j = 0; j < 10; j++) {
      grid[19][j] = 'X';
    }
    
    const linesCleared = board.clearCompleteLines();
    expect(linesCleared).toBe(1);
    
    // Bottom line should now be empty
    for (let j = 0; j < 10; j++) {
      expect(grid[19][j]).toBe('.');
    }
  });

  test('should handle piece rotation validation', () => {
    const shape = [
      [true, true, true, true] // I-piece
    ];
    const piece = new Piece(shape);
    piece.getPosition().setX(7); // Near right edge
    piece.getPosition().setY(17); // Near bottom
    
    // Rotation should fail due to boundary constraints
    const canRotate = board.canRotatePiece(piece, true);
    expect(canRotate).toBe(false);
    
    // Piece should be in original state
    expect(piece.getShape()).toEqual([[true, true, true, true]]);
  });
});