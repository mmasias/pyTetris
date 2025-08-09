#include "tetris/Console.h"
#include <sstream>
#include <cctype>
#include <stdexcept>
#include <limits>

namespace tetris {

Console::Console() {
    // Simplified check for ANSI support - assume most modern terminals support it
    // In practice, you might want more sophisticated detection
    supportsAnsiColors_ = true;
}

std::string Console::readString(const std::string& title) {
    if (!title.empty()) {
        write(title);
    }
    
    std::string input;
    std::getline(std::cin, input);
    return input;
}

int Console::readInt(const std::string& title) {
    while (true) {
        try {
            std::string input = readString(title);
            return std::stoi(input);
        } catch (const std::exception&) {
            writeError("integer");
        }
    }
}

double Console::readDouble(const std::string& title) {
    while (true) {
        try {
            std::string input = readString(title);
            return std::stod(input);
        } catch (const std::exception&) {
            writeError("double");
        }
    }
}

char Console::readChar(const std::string& title, bool transformToUpperCase) {
    while (true) {
        std::string input = readString(title);
        if (input.length() == 1) {
            char result = input[0];
            return transformToUpperCase ? std::toupper(result) : result;
        }
        writeError("character");
    }
}

void Console::write(const std::string& text) {
    std::cout << text;
}

void Console::write(const std::string& text, ForegroundColor color) {
    std::cout << applyForeground(text, color);
}

void Console::write(const std::string& text, BackgroundColor bgColor) {
    std::cout << applyBackground(text, bgColor);
}

void Console::write(const std::string& text, ForegroundColor fgColor, BackgroundColor bgColor) {
    std::cout << applyColors(text, fgColor, bgColor);
}

void Console::write(int value) {
    std::cout << value;
}

void Console::write(double value) {
    std::cout << value;
}

void Console::write(char character) {
    std::cout << character;
}

void Console::write(bool value) {
    std::cout << (value ? "true" : "false");
}

void Console::writeln() {
    std::cout << std::endl;
}

void Console::writeln(const std::string& text) {
    write(text);
    writeln();
}

void Console::writeln(const std::string& text, ForegroundColor color) {
    write(text, color);
    writeln();
}

void Console::writeln(const std::string& text, BackgroundColor bgColor) {
    write(text, bgColor);
    writeln();
}

void Console::writeln(const std::string& text, ForegroundColor fgColor, BackgroundColor bgColor) {
    write(text, fgColor, bgColor);
    writeln();
}

void Console::writeln(int value) {
    write(value);
    writeln();
}

void Console::writeln(double value) {
    write(value);
    writeln();
}

void Console::writeln(char character) {
    write(character);
    writeln();
}

void Console::writeln(bool value) {
    write(value);
    writeln();
}

void Console::clearScreen() {
    std::cout << "\033[H\033[2J" << std::flush;
}

void Console::writeError(const std::string& format) {
    writeln("FORMAT ERROR! Enter a " + format + " formatted value.", ForegroundColor::RED);
}

std::string Console::getForegroundCode(ForegroundColor color) const {
    switch (color) {
        case ForegroundColor::BLACK:  return "\u001B[30m";
        case ForegroundColor::RED:    return "\u001B[31m";
        case ForegroundColor::GREEN:  return "\u001B[32m";
        case ForegroundColor::YELLOW: return "\u001B[33m";
        case ForegroundColor::BLUE:   return "\u001B[34m";
        case ForegroundColor::PURPLE: return "\u001B[35m";
        case ForegroundColor::CYAN:   return "\u001B[36m";
        case ForegroundColor::WHITE:  return "\u001B[37m";
        case ForegroundColor::RESET:  return "\u001B[39m";
        default: return "";
    }
}

std::string Console::getBackgroundCode(BackgroundColor color) const {
    switch (color) {
        case BackgroundColor::BLACK:  return "\u001B[40m";
        case BackgroundColor::RED:    return "\u001B[41m";
        case BackgroundColor::GREEN:  return "\u001B[42m";
        case BackgroundColor::YELLOW: return "\u001B[43m";
        case BackgroundColor::BLUE:   return "\u001B[44m";
        case BackgroundColor::PURPLE: return "\u001B[45m";
        case BackgroundColor::CYAN:   return "\u001B[46m";
        case BackgroundColor::WHITE:  return "\u001B[47m";
        case BackgroundColor::RESET:  return "\u001B[49m";
        default: return "";
    }
}

std::string Console::applyForeground(const std::string& text, ForegroundColor color) const {
    if (!supportsAnsiColors_) {
        return text;
    }
    return getForegroundCode(color) + text + RESET_ALL;
}

std::string Console::applyBackground(const std::string& text, BackgroundColor color) const {
    if (!supportsAnsiColors_) {
        return text;
    }
    return getBackgroundCode(color) + text + RESET_ALL;
}

std::string Console::applyColors(const std::string& text, ForegroundColor fgColor, BackgroundColor bgColor) const {
    if (!supportsAnsiColors_) {
        return text;
    }
    return getForegroundCode(fgColor) + getBackgroundCode(bgColor) + text + RESET_ALL;
}

} // namespace tetris