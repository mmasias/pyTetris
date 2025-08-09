use std::io::{self, Write};
use crossterm::{
    cursor,
    execute,
    style::{Color, Print, ResetColor, SetForegroundColor, SetBackgroundColor},
    terminal::{Clear, ClearType},
};

/// Console colors for foreground text
#[derive(Debug, Clone, Copy)]
pub enum ForegroundColor {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Purple,
    Cyan,
    White,
    Reset,
}

impl From<ForegroundColor> for Color {
    fn from(color: ForegroundColor) -> Self {
        match color {
            ForegroundColor::Black => Color::Black,
            ForegroundColor::Red => Color::Red,
            ForegroundColor::Green => Color::Green,
            ForegroundColor::Yellow => Color::Yellow,
            ForegroundColor::Blue => Color::Blue,
            ForegroundColor::Purple => Color::Magenta,
            ForegroundColor::Cyan => Color::Cyan,
            ForegroundColor::White => Color::White,
            ForegroundColor::Reset => Color::Reset,
        }
    }
}

/// Console colors for background
#[derive(Debug, Clone, Copy)]
pub enum BackgroundColor {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Purple,
    Cyan,
    White,
    Reset,
}

impl From<BackgroundColor> for Color {
    fn from(color: BackgroundColor) -> Self {
        match color {
            BackgroundColor::Black => Color::Black,
            BackgroundColor::Red => Color::Red,
            BackgroundColor::Green => Color::Green,
            BackgroundColor::Yellow => Color::Yellow,
            BackgroundColor::Blue => Color::Blue,
            BackgroundColor::Purple => Color::Magenta,
            BackgroundColor::Cyan => Color::Cyan,
            BackgroundColor::White => Color::White,
            BackgroundColor::Reset => Color::Reset,
        }
    }
}

/// Console utility for input/output operations
pub struct Console {
    supports_ansi_colors: bool,
}

impl Console {
    /// Creates a new console instance
    pub fn new() -> Self {
        Console {
            supports_ansi_colors: true, // crossterm handles this automatically
        }
    }

    /// Reads a string from the user with an optional prompt
    pub fn read_string(&self, prompt: &str) -> Result<String, io::Error> {
        if !prompt.is_empty() {
            self.write(prompt)?;
        }
        
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        Ok(input.trim().to_string())
    }

    /// Reads an integer from the user with error handling
    #[allow(dead_code)]
    pub fn read_int(&self, prompt: &str) -> Result<i32, io::Error> {
        loop {
            match self.read_string(prompt) {
                Ok(input) => {
                    match input.parse::<i32>() {
                        Ok(value) => return Ok(value),
                        Err(_) => {
                            self.write_error("integer")?;
                        }
                    }
                }
                Err(e) => return Err(e),
            }
        }
    }

    /// Reads a double from the user with error handling
    pub fn read_double(&self, prompt: &str) -> Result<f64, io::Error> {
        loop {
            match self.read_string(prompt) {
                Ok(input) => {
                    match input.parse::<f64>() {
                        Ok(value) => return Ok(value),
                        Err(_) => {
                            self.write_error("double")?;
                        }
                    }
                }
                Err(e) => return Err(e),
            }
        }
    }

    /// Reads a single character from the user
    pub fn read_char(&self, prompt: &str, to_uppercase: bool) -> Result<char, io::Error> {
        loop {
            match self.read_string(prompt) {
                Ok(input) => {
                    let chars: Vec<char> = input.chars().collect();
                    if chars.len() == 1 {
                        let ch = chars[0];
                        return Ok(if to_uppercase { ch.to_ascii_uppercase() } else { ch });
                    } else {
                        self.write_error("character")?;
                    }
                }
                Err(e) => return Err(e),
            }
        }
    }

    /// Writes text to stdout
    pub fn write(&self, text: &str) -> Result<(), io::Error> {
        print!("{text}");
        io::stdout().flush()
    }

    /// Writes a line to stdout
    pub fn writeln(&self, text: &str) -> Result<(), io::Error> {
        println!("{text}");
        io::stdout().flush()
    }

    /// Writes colored text to stdout
    pub fn write_colored(&self, text: &str, fg_color: ForegroundColor) -> Result<(), io::Error> {
        if self.supports_ansi_colors {
            execute!(
                io::stdout(),
                SetForegroundColor(fg_color.into()),
                Print(text),
                ResetColor
            )?;
        } else {
            self.write(text)?;
        }
        Ok(())
    }

    /// Writes colored text with background to stdout
    pub fn write_colored_bg(&self, text: &str, fg_color: ForegroundColor, bg_color: BackgroundColor) -> Result<(), io::Error> {
        if self.supports_ansi_colors {
            execute!(
                io::stdout(),
                SetForegroundColor(fg_color.into()),
                SetBackgroundColor(bg_color.into()),
                Print(text),
                ResetColor
            )?;
        } else {
            self.write(text)?;
        }
        Ok(())
    }

    /// Writes a colored line to stdout
    pub fn writeln_colored(&self, text: &str, fg_color: ForegroundColor) -> Result<(), io::Error> {
        self.write_colored(text, fg_color)?;
        println!();
        Ok(())
    }

    /// Writes a colored line with background to stdout
    pub fn writeln_colored_bg(&self, text: &str, fg_color: ForegroundColor, bg_color: BackgroundColor) -> Result<(), io::Error> {
        self.write_colored_bg(text, fg_color, bg_color)?;
        println!();
        Ok(())
    }

    /// Clears the screen
    pub fn clear_screen(&self) -> Result<(), io::Error> {
        execute!(
            io::stdout(),
            Clear(ClearType::All),
            cursor::MoveTo(0, 0)
        )?;
        Ok(())
    }

    /// Writes an error message
    fn write_error(&self, format_type: &str) -> Result<(), io::Error> {
        self.writeln_colored(
            &format!("FORMAT ERROR! Enter a {format_type} formatted value."),
            ForegroundColor::Red
        )
    }
}

impl Default for Console {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_console_creation() {
        let console = Console::new();
        assert!(console.supports_ansi_colors);
    }

    #[test]
    fn test_color_conversion() {
        let fg_color: Color = ForegroundColor::Red.into();
        let bg_color: Color = BackgroundColor::Blue.into();
        
        // Basic validation that conversion works
        assert!(matches!(fg_color, Color::Red));
        assert!(matches!(bg_color, Color::Blue));
    }

    #[test]
    fn test_write_operations() {
        let console = Console::new();
        
        // These tests would require stdin/stdout mocking for proper testing
        // For now, just ensure methods can be called without panicking
        assert!(console.write("test").is_ok());
    }
}