import * as readline from 'readline';

/**
 * Console utilities for input/output operations.
 * Simplified version of the Java Console class for Node.js.
 */
export class Console {
  constructor() {
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });
  }

  /**
   * ANSI color codes for text formatting.
   */
  static Colors = {
    BLACK: '\x1b[30m',
    RED: '\x1b[31m',
    GREEN: '\x1b[32m',
    YELLOW: '\x1b[33m',
    BLUE: '\x1b[34m',
    PURPLE: '\x1b[35m',
    CYAN: '\x1b[36m',
    WHITE: '\x1b[37m',
    RESET: '\x1b[0m'
  };

  /**
   * Reads a string from the user with an optional prompt.
   * @param {string} [prompt=''] - The prompt to display
   * @returns {Promise<string>} The user input
   */
  readString(prompt = '') {
    return new Promise((resolve) => {
      this.rl.question(prompt, (input) => {
        resolve(input);
      });
    });
  }

  /**
   * Reads an integer from the user with validation.
   * @param {string} prompt - The prompt to display
   * @returns {Promise<number>} The integer value
   */
  async readInt(prompt) {
    let input;
    let isValid = false;
    
    do {
      try {
        const stringInput = await this.readString(prompt);
        input = parseInt(stringInput, 10);
        if (!isNaN(input)) {
          isValid = true;
        } else {
          this.writeError('integer');
        }
      } catch {
        this.writeError('integer');
      }
    } while (!isValid);
    
    return input;
  }

  /**
   * Reads a character from the user.
   * @param {string} prompt - The prompt to display
   * @returns {Promise<string>} The character
   */
  async readChar(prompt) {
    let char;
    let isValid = false;
    
    do {
      const input = await this.readString(prompt);
      if (input.length === 1) {
        char = input;
        isValid = true;
      } else {
        this.writeError('character');
      }
    } while (!isValid);
    
    return char;
  }

  /**
   * Writes text to the console.
   * @param {string|number} text - The text to write
   * @param {string} [color] - Optional color code
   */
  write(text, color = '') {
    if (color) {
      process.stdout.write(color + text + Console.Colors.RESET);
    } else {
      process.stdout.write(String(text));
    }
  }

  /**
   * Writes a line to the console.
   * @param {string|number} [text=''] - The text to write
   * @param {string} [color] - Optional color code
   */
  writeln(text = '', color = '') {
    if (color) {
      console.log(color + text + Console.Colors.RESET);
    } else {
      console.log(text);
    }
  }

  /**
   * Clears the console screen.
   */
  clearScreen() {
    console.clear();
  }

  /**
   * Writes an error message.
   * @param {string} format - The expected format type
   */
  writeError(format) {
    this.writeln(`FORMAT ERROR! Enter a ${format} formatted value.`, Console.Colors.RED);
  }

  /**
   * Closes the readline interface.
   */
  close() {
    this.rl.close();
  }
}