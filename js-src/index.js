import { Tetris } from './tetris.js';

/**
 * Main entry point for the JavaScript Tetris game.
 * Converted from Java Game class.
 */
async function main() {
  const game = new Tetris(10, 20);
  await game.gameLoop();
}

// Run the game
main().catch(console.error);