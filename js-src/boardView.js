/**
 * Handles the visual display of the game board.
 * Converted from Java BoardView class.
 */
export class BoardView {
  /**
   * Creates a new BoardView instance.
   * @param {Console} console - The console instance for output
   */
  constructor(console) {
    this.console = console;
  }

  /**
   * Displays the board with the current piece.
   * @param {Board} board - The board to display
   * @param {PieceView|null} currentPieceView - The current piece view
   */
  display(board, currentPieceView) {
    const width = board.getWidth();
    const height = board.getHeight();
    
    // Create a copy of the grid for display
    const displayGrid = Array(height).fill(null).map(() => Array(width).fill('.'));
    
    const grid = board.getGrid();
    for (let i = 0; i < height; i++) {
      for (let j = 0; j < width; j++) {
        displayGrid[i][j] = grid[i][j];
      }
    }

    // Render current piece if available
    if (currentPieceView && board.getCurrentPiece()) {
      currentPieceView.render(board.getCurrentPiece(), displayGrid);
    }

    // Clear screen and display board
    this.console.clearScreen();
    
    // Top border
    this.console.writeln('<!' + '='.repeat(((width + 2) * 2) - 1) + '!>');
    
    // Board content
    for (let i = 0; i < height; i++) {
      let line = '<!  ';
      for (let j = 0; j < width; j++) {
        line += displayGrid[i][j] + ' ';
      }
      line += ' !>';
      this.console.writeln(line);
    }
    
    // Bottom border
    this.console.writeln('<!' + '='.repeat(((width + 2) * 2) - 1) + '!>');
    this.console.writeln('\\/'.repeat(width + 4));
  }
}