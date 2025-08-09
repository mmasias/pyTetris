# JavaScript Tetris

A console-based Tetris game converted from Java to JavaScript with modern best practices.

## 🎮 Features

- **Classic Tetris gameplay** in ASCII console format
- **Four piece types**: I, O, T, L pieces
- **Full controls**: Movement (left/right), rotation (clockwise/counterclockwise)  
- **Line clearing** with score tracking
- **Collision detection** and game over conditions
- **Modern JavaScript** with ES6+ modules
- **Comprehensive testing** with Jest
- **Code quality** with ESLint

## 🚀 Quick Start

### Prerequisites
- Node.js 16+ (for ES modules support)
- npm

### Installation
```bash
npm install
```

### Run the Game
```bash
npm start
```

### Controls
- `4` - Move left
- `6` - Move right  
- `7` - Rotate counterclockwise (↺)
- `9` - Rotate clockwise (↻)
- `Enter` - Confirm move

## 🧪 Development

### Run Tests
```bash
npm test
```

### Code Quality Check
```bash
npm run lint
```

### Watch Mode for Tests
```bash
npm run test:watch
```

## 📁 Project Structure

```
├── js-src/              # JavaScript source code
│   ├── index.js         # Main entry point
│   ├── tetris.js        # Main game logic
│   ├── board.js         # Game board management
│   ├── piece.js         # Tetris piece logic
│   ├── pieceFactory.js  # Piece creation factory
│   ├── pieceView.js     # Piece rendering
│   ├── boardView.js     # Board rendering
│   ├── console.js       # Console I/O utilities
│   └── position.js      # 2D position handling
├── test/                # Test files
├── src/                 # Original Java source
└── CONVERSION.md        # Detailed conversion documentation
```

## 🔄 Conversion from Java

This project is a faithful conversion of a Java Tetris implementation to modern JavaScript. Key improvements include:

- **ES6+ modules** for better code organization
- **Comprehensive test suite** (27 tests covering all core functionality)
- **Modern async/await** for user input handling
- **JSDoc documentation** for all classes and methods
- **ESLint configuration** for code quality
- **npm scripts** for development workflow

For detailed conversion documentation, see [CONVERSION.md](CONVERSION.md).

## 🎯 Game Rules

1. Pieces fall from the top of the board
2. Use controls to move and rotate pieces as they fall
3. Complete horizontal lines to clear them and score points
4. Game ends when pieces reach the top
5. Score: 100 points per line cleared

## 🛠 Technical Details

- **Language**: JavaScript (ES2022+)
- **Runtime**: Node.js
- **Testing**: Jest with ES modules support
- **Linting**: ESLint with modern configuration
- **Module System**: Native ES6 imports/exports

## 📜 License

ISC

---

*Converted from Java implementation while preserving all original game mechanics and adding modern JavaScript best practices.*