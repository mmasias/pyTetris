# Simple test framework - not using Google Test to keep dependencies minimal

find_package(Threads REQUIRED)

# Test executable
add_executable(tetris_tests
    test_main.cpp
    test_Position.cpp
    test_Piece.cpp
    test_Board.cpp
)

# Link with the source files (without main)
target_sources(tetris_tests PRIVATE
    ../src/Position.cpp
    ../src/Piece.cpp
    ../src/Board.cpp
    ../src/Console.cpp
    ../src/PieceView.cpp
    ../src/BoardView.cpp
    ../src/PieceFactory.cpp
    ../src/Tetris.cpp
)

target_include_directories(tetris_tests PRIVATE ../include)
target_link_libraries(tetris_tests Threads::Threads)

# Add test
add_test(NAME tetris_unit_tests COMMAND tetris_tests)