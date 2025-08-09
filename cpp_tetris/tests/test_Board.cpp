#include "tetris/Board.h"
#include "tetris/PieceFactory.h"
#include <iostream>
#include <stdexcept>
#include <string>

// Test framework from test_main.cpp
extern int tests_run;
extern int tests_passed;
#define TEST(name) \
    void test_##name(); \
    void run_test_##name() { \
        std::cout << "Running test: " << #name << "... "; \
        tests_run++; \
        try { \
            test_##name(); \
            std::cout << "PASSED" << std::endl; \
            tests_passed++; \
        } catch (const std::exception& e) { \
            std::cout << "FAILED: " << e.what() << std::endl; \
        } catch (...) { \
            std::cout << "FAILED: Unknown exception" << std::endl; \
        } \
    } \
    void test_##name()

#define ASSERT_EQ(expected, actual) \
    if ((expected) != (actual)) { \
        throw std::runtime_error(std::string("Expected ") + std::to_string(expected) + \
                                " but got " + std::to_string(actual)); \
    }

#define ASSERT_TRUE(condition) \
    if (!(condition)) { \
        throw std::runtime_error(std::string("Expected true but got false")); \
    }

#define ASSERT_FALSE(condition) \
    if (condition) { \
        throw std::runtime_error(std::string("Expected false but got true")); \
    }

TEST(board_creation) {
    tetris::Board board(10, 20);
    
    ASSERT_EQ(10, board.getWidth());
    ASSERT_EQ(20, board.getHeight());
    
    // Check that board is initially empty
    const auto& grid = board.getGrid();
    for (int i = 0; i < 20; ++i) {
        for (int j = 0; j < 10; ++j) {
            ASSERT_EQ('.', grid[i][j]);
        }
    }
}

TEST(board_piece_placement) {
    tetris::Board board(10, 20);
    auto piece = tetris::PieceFactory::createOPiece();
    
    piece->getPosition().setX(0);
    piece->getPosition().setY(18);  // Near bottom
    
    board.placePiece(*piece, 'O');
    
    const auto& grid = board.getGrid();
    ASSERT_EQ('O', grid[18][0]);
    ASSERT_EQ('O', grid[18][1]);
    ASSERT_EQ('O', grid[19][0]);
    ASSERT_EQ('O', grid[19][1]);
}

TEST(board_collision_detection) {
    tetris::Board board(10, 20);
    auto piece = tetris::PieceFactory::createOPiece();
    
    // Test boundaries
    piece->getPosition().setX(-1);
    piece->getPosition().setY(0);
    ASSERT_FALSE(board.canMovePiece(*piece, 0, 0));
    
    piece->getPosition().setX(9);  // O-piece is 2x2, so x=9 is out of bounds
    piece->getPosition().setY(0);
    ASSERT_FALSE(board.canMovePiece(*piece, 0, 0));
    
    piece->getPosition().setX(0);
    piece->getPosition().setY(19);  // O-piece is 2x2, so y=19 is out of bounds
    ASSERT_FALSE(board.canMovePiece(*piece, 0, 0));
    
    // Test valid position
    piece->getPosition().setX(4);
    piece->getPosition().setY(10);
    ASSERT_TRUE(board.canMovePiece(*piece, 0, 0));
}

TEST(board_line_clearing) {
    tetris::Board board(3, 3);  // Small board for testing
    
    // Fill bottom line manually
    auto& grid = const_cast<std::vector<std::vector<char>>&>(board.getGrid());
    grid[2][0] = 'X';
    grid[2][1] = 'X';
    grid[2][2] = 'X';
    
    int linesCleared = board.clearCompleteLines();
    ASSERT_EQ(1, linesCleared);
    
    // Check that the line was cleared and moved down
    const auto& newGrid = board.getGrid();
    ASSERT_EQ('.', newGrid[2][0]);
    ASSERT_EQ('.', newGrid[2][1]);
    ASSERT_EQ('.', newGrid[2][2]);
}