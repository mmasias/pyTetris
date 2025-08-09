"""Console module - simplified console I/O with color support."""

import os
import sys
from enum import Enum
from typing import Union

try:
    import colorama
    colorama.init()
    HAS_COLORAMA = True
except ImportError:
    HAS_COLORAMA = False


class ForegroundColor(Enum):
    """ANSI foreground color codes."""
    BLACK = "\033[30m"
    RED = "\033[31m"
    GREEN = "\033[32m"
    YELLOW = "\033[33m"
    BLUE = "\033[34m"
    PURPLE = "\033[35m"
    CYAN = "\033[36m"
    WHITE = "\033[37m"
    RESET = "\033[39m"


class BackgroundColor(Enum):
    """ANSI background color codes."""
    BLACK = "\033[40m"
    RED = "\033[41m"
    GREEN = "\033[42m"
    YELLOW = "\033[43m"
    BLUE = "\033[44m"
    PURPLE = "\033[45m"
    CYAN = "\033[46m"
    WHITE = "\033[47m"
    RESET = "\033[49m"


class Console:
    """Simple console I/O class with color support."""
    
    RESET_ALL = "\033[0m"
    
    def __init__(self):
        """Initialize console with color support detection."""
        self.supports_ansi_colors = self._supports_colors()
    
    def _supports_colors(self) -> bool:
        """Check if terminal supports ANSI colors.
        
        Returns:
            True if colors are supported
        """
        if HAS_COLORAMA:
            return True
        
        # Check if running in a terminal that supports colors
        if hasattr(sys.stdout, 'isatty') and sys.stdout.isatty():
            if os.name == 'nt':  # Windows
                return False  # Unless colorama is available
            return True
        return False
    
    def _apply_foreground(self, text: str, color: ForegroundColor) -> str:
        """Apply foreground color to text.
        
        Args:
            text: Text to colorize
            color: Foreground color
            
        Returns:
            Colorized text if supported, plain text otherwise
        """
        if not self.supports_ansi_colors:
            return text
        return color.value + text + self.RESET_ALL
    
    def _apply_background(self, text: str, color: BackgroundColor) -> str:
        """Apply background color to text.
        
        Args:
            text: Text to colorize
            color: Background color
            
        Returns:
            Colorized text if supported, plain text otherwise
        """
        if not self.supports_ansi_colors:
            return text
        return color.value + text + self.RESET_ALL
    
    def _apply_colors(self, text: str, fg_color: ForegroundColor, 
                     bg_color: BackgroundColor) -> str:
        """Apply both foreground and background colors.
        
        Args:
            text: Text to colorize
            fg_color: Foreground color
            bg_color: Background color
            
        Returns:
            Colorized text if supported, plain text otherwise
        """
        if not self.supports_ansi_colors:
            return text
        return fg_color.value + bg_color.value + text + self.RESET_ALL
    
    def read_string(self, prompt: str = "") -> str:
        """Read a string from user input.
        
        Args:
            prompt: Prompt to display
            
        Returns:
            User input string
        """
        try:
            return input(prompt)
        except (EOFError, KeyboardInterrupt):
            return ""
    
    def read_int(self, prompt: str) -> int:
        """Read an integer from user input with validation.
        
        Args:
            prompt: Prompt to display
            
        Returns:
            User input integer
        """
        while True:
            try:
                return int(self.read_string(prompt))
            except ValueError:
                self.write_error("integer")
    
    def read_double(self, prompt: str) -> float:
        """Read a float from user input with validation.
        
        Args:
            prompt: Prompt to display
            
        Returns:
            User input float
        """
        while True:
            try:
                return float(self.read_string(prompt))
            except ValueError:
                self.write_error("double")
    
    def read_char(self, prompt: str, transform_to_upper: bool = False) -> str:
        """Read a single character from user input.
        
        Args:
            prompt: Prompt to display
            transform_to_upper: Whether to convert to uppercase
            
        Returns:
            Single character string
        """
        while True:
            input_str = self.read_string(prompt)
            if len(input_str) == 1:
                return input_str.upper() if transform_to_upper else input_str
            self.write_error("character")
    
    def write(self, value: Union[str, int, float, bool], 
              fg_color: ForegroundColor = None,
              bg_color: BackgroundColor = None) -> None:
        """Write value to console with optional colors.
        
        Args:
            value: Value to write
            fg_color: Optional foreground color
            bg_color: Optional background color
        """
        text = str(value)
        
        if fg_color and bg_color:
            text = self._apply_colors(text, fg_color, bg_color)
        elif fg_color:
            text = self._apply_foreground(text, fg_color)
        elif bg_color:
            text = self._apply_background(text, bg_color)
        
        print(text, end="")
    
    def writeln(self, value: Union[str, int, float, bool] = "", 
                fg_color: ForegroundColor = None,
                bg_color: BackgroundColor = None) -> None:
        """Write value to console with newline and optional colors.
        
        Args:
            value: Value to write
            fg_color: Optional foreground color
            bg_color: Optional background color
        """
        self.write(value, fg_color, bg_color)
        print()
    
    def clear_screen(self) -> None:
        """Clear the console screen."""
        if os.name == 'nt':  # Windows
            os.system('cls')
        else:  # Unix/Linux/MacOS
            os.system('clear')
    
    def write_error(self, format_type: str) -> None:
        """Write a format error message.
        
        Args:
            format_type: Type of format error
        """
        self.writeln(f"FORMAT ERROR! Enter a {format_type} formatted value.", 
                    ForegroundColor.RED)