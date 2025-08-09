#pragma once

#include <string>
#include <iostream>

namespace tetris {

/**
 * @brief Console I/O class with ANSI color support
 * 
 * Provides colorized console output and input functionality
 * similar to the original Java Console class.
 */
class Console {
public:
    /**
     * @brief ANSI foreground color codes
     */
    enum class ForegroundColor {
        BLACK,
        RED,
        GREEN,
        YELLOW,
        BLUE,
        PURPLE,
        CYAN,
        WHITE,
        RESET
    };

    /**
     * @brief ANSI background color codes
     */
    enum class BackgroundColor {
        BLACK,
        RED,
        GREEN,
        YELLOW,
        BLUE,
        PURPLE,
        CYAN,
        WHITE,
        RESET
    };

    /**
     * @brief Construct a new Console object
     */
    Console();

    /**
     * @brief Read a string from input with optional prompt
     * @param title Prompt message to display
     * @return std::string The input string
     */
    std::string readString(const std::string& title = "");

    /**
     * @brief Read an integer from input with prompt
     * @param title Prompt message to display
     * @return int The input integer
     */
    int readInt(const std::string& title);

    /**
     * @brief Read a double from input with prompt
     * @param title Prompt message to display
     * @return double The input double
     */
    double readDouble(const std::string& title);

    /**
     * @brief Read a character from input with prompt
     * @param title Prompt message to display
     * @param transformToUpperCase Whether to convert to uppercase
     * @return char The input character
     */
    char readChar(const std::string& title, bool transformToUpperCase = false);

    /**
     * @brief Write text to console
     * @param text Text to write
     */
    void write(const std::string& text);

    /**
     * @brief Write text with foreground color
     * @param text Text to write
     * @param color Foreground color
     */
    void write(const std::string& text, ForegroundColor color);

    /**
     * @brief Write text with background color
     * @param text Text to write
     * @param bgColor Background color
     */
    void write(const std::string& text, BackgroundColor bgColor);

    /**
     * @brief Write text with both foreground and background colors
     * @param text Text to write
     * @param fgColor Foreground color
     * @param bgColor Background color
     */
    void write(const std::string& text, ForegroundColor fgColor, BackgroundColor bgColor);

    /**
     * @brief Write various data types
     */
    void write(int value);
    void write(double value);
    void write(char character);
    void write(bool value);

    /**
     * @brief Write line (text + newline)
     */
    void writeln();
    void writeln(const std::string& text);
    void writeln(const std::string& text, ForegroundColor color);
    void writeln(const std::string& text, BackgroundColor bgColor);
    void writeln(const std::string& text, ForegroundColor fgColor, BackgroundColor bgColor);
    void writeln(int value);
    void writeln(double value);
    void writeln(char character);
    void writeln(bool value);

    /**
     * @brief Clear the console screen
     */
    void clearScreen();

    /**
     * @brief Write error message in red
     * @param format The expected format type
     */
    void writeError(const std::string& format);

private:
    /**
     * @brief Get ANSI color code for foreground color
     * @param color The foreground color
     * @return std::string The ANSI code
     */
    std::string getForegroundCode(ForegroundColor color) const;

    /**
     * @brief Get ANSI color code for background color
     * @param color The background color
     * @return std::string The ANSI code
     */
    std::string getBackgroundCode(BackgroundColor color) const;

    /**
     * @brief Apply foreground color to text
     * @param text The text to colorize
     * @param color The foreground color
     * @return std::string Colorized text
     */
    std::string applyForeground(const std::string& text, ForegroundColor color) const;

    /**
     * @brief Apply background color to text
     * @param text The text to colorize
     * @param color The background color
     * @return std::string Colorized text
     */
    std::string applyBackground(const std::string& text, BackgroundColor color) const;

    /**
     * @brief Apply both foreground and background colors to text
     * @param text The text to colorize
     * @param fgColor The foreground color
     * @param bgColor The background color
     * @return std::string Colorized text
     */
    std::string applyColors(const std::string& text, ForegroundColor fgColor, BackgroundColor bgColor) const;

    bool supportsAnsiColors_;
    static constexpr const char* RESET_ALL = "\u001B[0m";
};

} // namespace tetris