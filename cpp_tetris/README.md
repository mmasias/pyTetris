# C++ Tetris Game

A modern C++17 implementation of the classic Tetris game, converted from Java with comprehensive testing and best practices.

## Features

- Classic Tetris gameplay with 10x20 board
- 4 piece types: I, O, T, L pieces
- Piece movement and rotation
- Line clearing and scoring
- Colorized console output
- Unit tests with custom testing framework

## Project Structure

```
cpp_tetris/
├── CMakeLists.txt          # Build configuration
├── README.md               # This file
├── include/tetris/         # Public headers
│   ├── Board.h
│   ├── BoardView.h
│   ├── Console.h
│   ├── Game.h
│   ├── Piece.h
│   ├── PieceFactory.h
│   ├── PieceView.h
│   ├── Position.h
│   └── Tetris.h
├── src/                    # Source implementations
│   ├── Board.cpp
│   ├── BoardView.cpp
│   ├── Console.cpp
│   ├── Game.cpp
│   ├── Piece.cpp
│   ├── PieceFactory.cpp
│   ├── PieceView.cpp
│   ├── Position.cpp
│   └── Tetris.cpp
├── tests/                  # Unit tests
│   ├── CMakeLists.txt
│   ├── test_main.cpp
│   ├── test_Position.cpp
│   ├── test_Piece.cpp
│   └── test_Board.cpp
└── docs/                   # Documentation
    └── conversion_log.md   # Detailed conversion process
```

## Requirements

- C++17 compatible compiler (GCC 7+, Clang 5+, MSVC 2017+)
- CMake 3.16+
- Terminal with ANSI color support

## Building

```bash
# Clone and navigate to the C++ directory
cd cpp_tetris

# Create build directory
mkdir build && cd build

# Configure with CMake
cmake ..

# Build the project
make -j4
```

## Running

### Play the Game
```bash
./tetris
```

### Run Tests
```bash
./tests/tetris_tests
```

### Run Tests with CMake/CTest
```bash
ctest
```

## Game Controls

- `4`: Move piece left
- `6`: Move piece right
- `7`: Rotate piece counter-clockwise
- `9`: Rotate piece clockwise
- `Enter`: Execute command

## Architecture

The game follows a clean Model-View separation:

- **Model Classes**: `Board`, `Piece`, `Position` - Core game logic
- **View Classes**: `BoardView`, `PieceView`, `Console` - Display and I/O
- **Controller**: `Tetris` - Game state and user input processing
- **Factory**: `PieceFactory` - Piece creation using Factory pattern

## Key Design Decisions

### Modern C++ Features
- **Smart Pointers**: `std::shared_ptr` and `std::unique_ptr` for memory management
- **RAII**: Automatic resource management
- **Const Correctness**: Proper const methods and parameters
- **Namespaces**: All classes in `tetris` namespace

### Type Safety
- **Strong Typing**: No implicit conversions
- **Bounds Checking**: Safe array access patterns
- **Exception Safety**: Proper error handling

### Testing
- Custom lightweight testing framework
- Unit tests for core functionality
- Integration tests for game flow

## Performance Considerations

- **Move Semantics**: Used for matrix operations (piece rotation)
- **Stack Allocation**: Preferred over heap when possible
- **Efficient Containers**: `std::vector` for dynamic arrays
- **Minimal Dependencies**: Only standard library used

## Cross-Platform Notes

### Terminal Support
- Colors work on most modern terminals (Linux, macOS, Windows Terminal)
- May not work properly on older Windows cmd.exe
- For best experience, use a modern terminal emulator

### Build System
- CMake provides cross-platform build configuration
- Tested on Linux (GCC) and should work on other platforms

## Differences from Java Version

### Improvements
- **Memory Safety**: No garbage collection needed, RAII handles cleanup
- **Performance**: Compiled code with potential optimizations
- **Type Safety**: Stronger compile-time guarantees
- **Testing**: Comprehensive unit test suite added

### Functional Equivalence
- All original game mechanics preserved
- Same piece types and behaviors
- Identical scoring system
- Same visual appearance

## Development

### Adding New Piece Types
1. Add shape definition in `PieceFactory.cpp`
2. Add corresponding view creation method
3. Update random piece selection in `Tetris.cpp`

### Extending Functionality
- Game state can be easily extended with new scoring rules
- Board size is configurable through constructor parameters
- Console colors can be customized in `Console.h`

## Testing

The test suite covers:
- **Position**: Coordinate operations and equality
- **Piece**: Movement, rotation, and shape integrity  
- **Board**: Collision detection, piece placement, line clearing

Run tests individually:
```bash
# All tests
./tests/tetris_tests

# With verbose output
./tests/tetris_tests --verbose
```

## Contributing

When contributing:
1. Follow the existing code style
2. Add tests for new functionality
3. Update documentation as needed
4. Ensure all tests pass before submitting

## License

This project is provided as-is for educational purposes. Based on the classic Tetris game concept.