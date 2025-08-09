import { Position } from '../js-src/position.js';

describe('Position', () => {
  test('should create a position with x and y coordinates', () => {
    const position = new Position(5, 10);
    expect(position.getX()).toBe(5);
    expect(position.getY()).toBe(10);
  });

  test('should set x coordinate', () => {
    const position = new Position(0, 0);
    position.setX(15);
    expect(position.getX()).toBe(15);
  });

  test('should set y coordinate', () => {
    const position = new Position(0, 0);
    position.setY(20);
    expect(position.getY()).toBe(20);
  });

  test('should add two positions correctly', () => {
    const pos1 = new Position(3, 4);
    const pos2 = new Position(2, 1);
    const result = pos1.add(pos2);
    
    expect(result.getX()).toBe(5);
    expect(result.getY()).toBe(5);
    
    // Original positions should remain unchanged
    expect(pos1.getX()).toBe(3);
    expect(pos1.getY()).toBe(4);
  });
});