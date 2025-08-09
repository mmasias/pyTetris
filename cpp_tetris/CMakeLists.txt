cmake_minimum_required(VERSION 3.16)
project(TetrisGame VERSION 1.0.0 LANGUAGES CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Add compiler flags for warnings and debugging
if(MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# Include directories
include_directories(include)

# Source files
set(SOURCES
    src/Position.cpp
    src/Piece.cpp
    src/Board.cpp
    src/Console.cpp
    src/PieceView.cpp
    src/BoardView.cpp
    src/PieceFactory.cpp
    src/Tetris.cpp
    src/Game.cpp
)

# Create the executable
add_executable(tetris ${SOURCES})

# Enable testing
enable_testing()

# Add subdirectory for tests
add_subdirectory(tests)

# Install target
install(TARGETS tetris DESTINATION bin)