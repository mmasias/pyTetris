import { Board } from './board.js';
import { BoardView } from './boardView.js';
import { Console } from './console.js';
import { PieceFactory } from './pieceFactory.js';

/**
 * Main Tetris game logic and game loop.
 * Converted from Java Tetris class.
 */
export class Tetris {
  /**
   * Creates a new Tetris game instance.
   * @param {number} width - The board width
   * @param {number} height - The board height
   */
  constructor(width, height) {
    this.console = new Console();
    this.board = new Board(width, height);
    this.boardView = new BoardView(this.console);
    this.gameRunning = true;
    this.score = 0;
    this.currentPieceView = null;
  }

  /**
   * Spawns a new random piece.
   */
  spawnNewPiece() {
    const pieces = [
      PieceFactory.createIPiece(),
      PieceFactory.createOPiece(),
      PieceFactory.createTPiece(),
      PieceFactory.createLPiece()
    ];

    const pieceViews = [
      PieceFactory.createIPieceView(),
      PieceFactory.createOPieceView(),
      PieceFactory.createTPieceView(),
      PieceFactory.createLPieceView()
    ];

    const randomIndex = Math.floor(Math.random() * pieces.length);
    this.board.setCurrentPiece(pieces[randomIndex]);
    this.currentPieceView = pieceViews[randomIndex];
  }

  /**
   * Processes user input commands.
   * @param {string} input - The user input
   */
  processUserInput(input) {
    if (input.length !== 0) {
      const command = input.charAt(0);
      const currentPiece = this.board.getCurrentPiece();

      switch (command) {
        case '4': // Move left
          if (this.board.canMovePiece(currentPiece, -1, 0)) {
            currentPiece.moveLeft();
          }
          break;
        case '6': // Move right
          if (this.board.canMovePiece(currentPiece, 1, 0)) {
            currentPiece.moveRight();
          }
          break;
        case '7': // Rotate counter-clockwise
          this.board.canRotatePiece(currentPiece, false);
          break;
        case '9': // Rotate clockwise
          this.board.canRotatePiece(currentPiece, true);
          break;
      }
    }
  }

  /**
   * Main game loop.
   */
  async gameLoop() {
    this.spawnNewPiece();
    
    while (this.gameRunning) {
      this.boardView.display(this.board, this.currentPieceView);
      
      const input = await this.console.readString(
        `Score: ${this.score}\n\nComando (4=izq, 6=der, 7=rotar↺, 9=rotar↻): `
      );
      
      this.processUserInput(input);

      const currentPiece = this.board.getCurrentPiece();

      // Try to move piece down
      if (this.board.canMovePiece(currentPiece, 0, 1)) {
        currentPiece.moveDown();
      } else {
        // Piece has landed, place it permanently
        this.board.placePiece(currentPiece, this.currentPieceView.getSymbol());

        // Check for completed lines
        const linesCleared = this.board.clearCompleteLines();
        this.score += linesCleared * 100;

        if (linesCleared > 0) {
          this.console.writeln(`¡${linesCleared} línea(s) eliminada(s)!`);
          await this.console.readString('Presiona Enter para continuar...');
        }

        // Spawn new piece
        this.spawnNewPiece();

        // Check for game over
        if (!this.board.canMovePiece(this.board.getCurrentPiece(), 0, 0)) {
          this.gameRunning = false;
          this.console.writeln('¡GAME OVER!');
          this.console.writeln(`Puntuación final: ${this.score}`);
        }
      }
    }

    this.console.close();
  }
}