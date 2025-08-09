#include <iostream>
#include <cassert>
#include <string>

// Simple test framework
int tests_run = 0;
int tests_passed = 0;

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

// Forward declarations of test functions
void run_test_position_creation();
void run_test_position_movement();
void run_test_position_equality();
void run_test_piece_creation();
void run_test_piece_movement();
void run_test_piece_rotation();
void run_test_board_creation();
void run_test_board_piece_placement();
void run_test_board_collision_detection();
void run_test_board_line_clearing();

int main() {
    std::cout << "Running Tetris C++ Tests" << std::endl;
    std::cout << "=========================" << std::endl;

    // Run all tests
    run_test_position_creation();
    run_test_position_movement();
    run_test_position_equality();
    run_test_piece_creation();
    run_test_piece_movement();
    run_test_piece_rotation();
    run_test_board_creation();
    run_test_board_piece_placement();
    run_test_board_collision_detection();
    run_test_board_line_clearing();

    // Summary
    std::cout << "\n=========================" << std::endl;
    std::cout << "Tests run: " << tests_run << std::endl;
    std::cout << "Tests passed: " << tests_passed << std::endl;
    std::cout << "Tests failed: " << (tests_run - tests_passed) << std::endl;

    return tests_passed == tests_run ? 0 : 1;
}