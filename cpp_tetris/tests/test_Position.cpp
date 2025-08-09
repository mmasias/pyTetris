#include "tetris/Position.h"
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

TEST(position_creation) {
    tetris::Position pos1;
    ASSERT_EQ(0, pos1.getX());
    ASSERT_EQ(0, pos1.getY());

    tetris::Position pos2(5, 10);
    ASSERT_EQ(5, pos2.getX());
    ASSERT_EQ(10, pos2.getY());
}

TEST(position_movement) {
    tetris::Position pos(3, 4);
    
    pos.setX(8);
    pos.setY(12);
    
    ASSERT_EQ(8, pos.getX());
    ASSERT_EQ(12, pos.getY());
}

TEST(position_equality) {
    tetris::Position pos1(5, 10);
    tetris::Position pos2(5, 10);
    tetris::Position pos3(5, 11);
    
    ASSERT_TRUE(pos1 == pos2);
    ASSERT_TRUE(pos1 != pos3);
}