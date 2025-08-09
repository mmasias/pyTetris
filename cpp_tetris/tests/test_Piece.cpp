#include "tetris/Piece.h"
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

TEST(piece_creation) {
    std::vector<std::vector<bool>> shape = {
        {true, true},
        {true, true}
    };
    
    tetris::Piece piece(shape);
    
    ASSERT_EQ(0, piece.getPosition().getX());
    ASSERT_EQ(0, piece.getPosition().getY());
    ASSERT_EQ(2, static_cast<int>(piece.getShape().size()));
    ASSERT_EQ(2, static_cast<int>(piece.getShape()[0].size()));
}

TEST(piece_movement) {
    std::vector<std::vector<bool>> shape = {{true}};
    tetris::Piece piece(shape);
    
    piece.moveRight();
    ASSERT_EQ(1, piece.getPosition().getX());
    ASSERT_EQ(0, piece.getPosition().getY());
    
    piece.moveDown();
    ASSERT_EQ(1, piece.getPosition().getX());
    ASSERT_EQ(1, piece.getPosition().getY());
    
    piece.moveLeft();
    ASSERT_EQ(0, piece.getPosition().getX());
    ASSERT_EQ(1, piece.getPosition().getY());
}

TEST(piece_rotation) {
    std::vector<std::vector<bool>> shape = {
        {true, true, true, true}  // I-piece
    };
    tetris::Piece piece(shape);
    
    // Original: 1x4
    ASSERT_EQ(1, static_cast<int>(piece.getShape().size()));
    ASSERT_EQ(4, static_cast<int>(piece.getShape()[0].size()));
    
    piece.rotateClockwise();
    
    // After rotation: 4x1
    ASSERT_EQ(4, static_cast<int>(piece.getShape().size()));
    ASSERT_EQ(1, static_cast<int>(piece.getShape()[0].size()));
    
    // Verify the rotated shape
    for (int i = 0; i < 4; ++i) {
        ASSERT_TRUE(piece.getShape()[i][0]);
    }
}