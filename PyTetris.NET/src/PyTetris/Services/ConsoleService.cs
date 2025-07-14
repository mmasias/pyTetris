namespace PyTetris.Services
{
    /// <summary>
    /// Console service for input/output operations with color support
    /// </summary>
    public class ConsoleService
    {
        public enum ForegroundColor
        {
            Black,
            Red,
            Green,
            Yellow,
            Blue,
            Purple,
            Cyan,
            White,
            Reset
        }

        public enum BackgroundColor
        {
            Black,
            Red,
            Green,
            Yellow,
            Blue,
            Purple,
            Cyan,
            White,
            Reset
        }

        private static readonly Dictionary<ForegroundColor, string> ForegroundCodes = new()
        {
            { ForegroundColor.Black, "\u001B[30m" },
            { ForegroundColor.Red, "\u001B[31m" },
            { ForegroundColor.Green, "\u001B[32m" },
            { ForegroundColor.Yellow, "\u001B[33m" },
            { ForegroundColor.Blue, "\u001B[34m" },
            { ForegroundColor.Purple, "\u001B[35m" },
            { ForegroundColor.Cyan, "\u001B[36m" },
            { ForegroundColor.White, "\u001B[37m" },
            { ForegroundColor.Reset, "\u001B[39m" }
        };

        private static readonly Dictionary<BackgroundColor, string> BackgroundCodes = new()
        {
            { BackgroundColor.Black, "\u001B[40m" },
            { BackgroundColor.Red, "\u001B[41m" },
            { BackgroundColor.Green, "\u001B[42m" },
            { BackgroundColor.Yellow, "\u001B[43m" },
            { BackgroundColor.Blue, "\u001B[44m" },
            { BackgroundColor.Purple, "\u001B[45m" },
            { BackgroundColor.Cyan, "\u001B[46m" },
            { BackgroundColor.White, "\u001B[47m" },
            { BackgroundColor.Reset, "\u001B[49m" }
        };

        private const string ResetAll = "\u001B[0m";
        private readonly bool supportsAnsiColors;

        public ConsoleService()
        {
            supportsAnsiColors = !OperatingSystem.IsWindows() || Environment.GetEnvironmentVariable("TERM") != null;
        }

        private string ApplyForeground(string text, ForegroundColor color)
        {
            return supportsAnsiColors ? ForegroundCodes[color] + text + ResetAll : text;
        }

        private string ApplyBackground(string text, BackgroundColor color)
        {
            return supportsAnsiColors ? BackgroundCodes[color] + text + ResetAll : text;
        }

        private string ApplyColors(string text, ForegroundColor foreground, BackgroundColor background)
        {
            if (!supportsAnsiColors)
                return text;
            return ForegroundCodes[foreground] + BackgroundCodes[background] + text + ResetAll;
        }

        public string ReadString(string title = "")
        {
            Write(title);
            return Console.ReadLine() ?? "";
        }

        public int ReadInt(string title)
        {
            int input;
            bool ok;
            do
            {
                try
                {
                    input = int.Parse(ReadString(title));
                    ok = true;
                }
                catch
                {
                    WriteError("integer");
                    ok = false;
                    input = 0;
                }
            } while (!ok);
            return input;
        }

        public double ReadDouble(string title)
        {
            double input;
            bool ok;
            do
            {
                try
                {
                    input = double.Parse(ReadString(title));
                    ok = true;
                }
                catch
                {
                    WriteError("double");
                    ok = false;
                    input = 0;
                }
            } while (!ok);
            return input;
        }

        public char ReadChar(string title, bool transformToUpperCase = false)
        {
            char charValue;
            bool ok;
            do
            {
                string input = ReadString(title);
                if (input.Length != 1)
                {
                    WriteError("character");
                    ok = false;
                    charValue = ' ';
                }
                else
                {
                    charValue = input[0];
                    ok = true;
                }
            } while (!ok);
            return transformToUpperCase ? char.ToUpper(charValue) : charValue;
        }

        public void Write(string text)
        {
            Console.Write(text);
        }

        public void Write(int value)
        {
            Write(value.ToString());
        }

        public void Write(double value)
        {
            Write(value.ToString());
        }

        public void Write(char character)
        {
            Write(character.ToString());
        }

        public void Write(bool value)
        {
            Write(value.ToString());
        }

        public void WriteLine()
        {
            Console.WriteLine();
        }

        public void WriteLine(string text)
        {
            Write(text);
            WriteLine();
        }

        public void WriteLine(int value)
        {
            Write(value);
            WriteLine();
        }

        public void WriteLine(double value)
        {
            Write(value);
            WriteLine();
        }

        public void WriteLine(char value)
        {
            Write(value);
            WriteLine();
        }

        public void WriteLine(bool value)
        {
            Write(value);
            WriteLine();
        }

        public void ClearScreen()
        {
            Console.Clear();
        }

        public void Write(string text, ForegroundColor color)
        {
            Write(ApplyForeground(text, color));
        }

        public void WriteLine(string text, ForegroundColor color)
        {
            Write(text, color);
            WriteLine();
        }

        public void Write(string text, BackgroundColor bgColor)
        {
            Write(ApplyBackground(text, bgColor));
        }

        public void WriteLine(string text, BackgroundColor bgColor)
        {
            Write(text, bgColor);
            WriteLine();
        }

        public void Write(string text, ForegroundColor fgColor, BackgroundColor bgColor)
        {
            Write(ApplyColors(text, fgColor, bgColor));
        }

        public void WriteLine(string text, ForegroundColor fgColor, BackgroundColor bgColor)
        {
            Write(text, fgColor, bgColor);
            WriteLine();
        }

        public void WriteError(string format)
        {
            WriteLine($"FORMAT ERROR! Enter a {format} formatted value.", ForegroundColor.Red);
        }
    }
}