import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import static org.junit.jupiter.api.Assertions.*;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

public class ConsoleTest {
    
    private Console console;
    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final PrintStream originalOut = System.out;
    
    @BeforeEach
    public void setUp() {
        console = new Console();
        System.setOut(new PrintStream(outContent));
    }
    
    @AfterEach
    public void restoreStream() {
        System.setOut(originalOut);
    }
    
    @Test
    public void testWriteString() {
        console.write("Hello World");
        assertEquals("Hello World", outContent.toString());
    }
    
    @Test
    public void testWriteInt() {
        console.write(42);
        assertEquals("42", outContent.toString());
    }
    
    @Test
    public void testWriteDouble() {
        console.write(3.14);
        assertEquals("3.14", outContent.toString());
    }
    
    @Test
    public void testWriteChar() {
        console.write('A');
        assertEquals("A", outContent.toString());
    }
    
    @Test
    public void testWriteBoolean() {
        console.write(true);
        assertEquals("true", outContent.toString());
    }
    
    @Test
    public void testWritelnString() {
        console.writeln("Hello World");
        assertEquals("Hello World" + System.lineSeparator(), outContent.toString());
    }
    
    @Test
    public void testWritelnInt() {
        console.writeln(42);
        assertEquals("42" + System.lineSeparator(), outContent.toString());
    }
    
    @Test
    public void testWritelnDouble() {
        console.writeln(3.14);
        assertEquals("3.14" + System.lineSeparator(), outContent.toString());
    }
    
    @Test
    public void testWritelnChar() {
        console.writeln('A');
        assertEquals("A" + System.lineSeparator(), outContent.toString());
    }
    
    @Test
    public void testWritelnBoolean() {
        console.writeln(true);
        assertEquals("true" + System.lineSeparator(), outContent.toString());
    }
    
    @Test
    public void testWritelnEmpty() {
        console.writeln();
        assertEquals(System.lineSeparator(), outContent.toString());
    }
    
    @Test
    public void testWriteError() {
        console.writeError("integer");
        String expected = "FORMAT ERROR! Enter a integer formatted value." + System.lineSeparator();
        assertEquals(expected, outContent.toString());
    }
    
    @Test
    public void testForegroundColorEnum() {
        assertEquals("\u001B[30m", Console.ForegroundColor.BLACK.getCode());
        assertEquals("\u001B[31m", Console.ForegroundColor.RED.getCode());
        assertEquals("\u001B[32m", Console.ForegroundColor.GREEN.getCode());
        assertEquals("\u001B[33m", Console.ForegroundColor.YELLOW.getCode());
        assertEquals("\u001B[34m", Console.ForegroundColor.BLUE.getCode());
        assertEquals("\u001B[35m", Console.ForegroundColor.PURPLE.getCode());
        assertEquals("\u001B[36m", Console.ForegroundColor.CYAN.getCode());
        assertEquals("\u001B[37m", Console.ForegroundColor.WHITE.getCode());
        assertEquals("\u001B[39m", Console.ForegroundColor.RESET.getCode());
    }
    
    @Test
    public void testBackgroundColorEnum() {
        assertEquals("\u001B[40m", Console.BackgroundColor.BLACK.getCode());
        assertEquals("\u001B[41m", Console.BackgroundColor.RED.getCode());
        assertEquals("\u001B[42m", Console.BackgroundColor.GREEN.getCode());
        assertEquals("\u001B[43m", Console.BackgroundColor.YELLOW.getCode());
        assertEquals("\u001B[44m", Console.BackgroundColor.BLUE.getCode());
        assertEquals("\u001B[45m", Console.BackgroundColor.PURPLE.getCode());
        assertEquals("\u001B[46m", Console.BackgroundColor.CYAN.getCode());
        assertEquals("\u001B[47m", Console.BackgroundColor.WHITE.getCode());
        assertEquals("\u001B[49m", Console.BackgroundColor.RESET.getCode());
    }
    
    @Test
    public void testWriteWithForegroundColor() {
        console.write("Test", Console.ForegroundColor.RED);
        // On systems that don't support ANSI colors, it should just print the text
        assertTrue(outContent.toString().contains("Test"));
    }
    
    @Test
    public void testWritelnWithForegroundColor() {
        console.writeln("Test", Console.ForegroundColor.RED);
        // On systems that don't support ANSI colors, it should just print the text with newline
        assertTrue(outContent.toString().contains("Test"));
        assertTrue(outContent.toString().contains(System.lineSeparator()));
    }
    
    @Test
    public void testWriteWithBackgroundColor() {
        console.write("Test", Console.BackgroundColor.YELLOW);
        // On systems that don't support ANSI colors, it should just print the text
        assertTrue(outContent.toString().contains("Test"));
    }
    
    @Test
    public void testWritelnWithBackgroundColor() {
        console.writeln("Test", Console.BackgroundColor.YELLOW);
        // On systems that don't support ANSI colors, it should just print the text with newline
        assertTrue(outContent.toString().contains("Test"));
        assertTrue(outContent.toString().contains(System.lineSeparator()));
    }
    
    @Test
    public void testWriteWithBothColors() {
        console.write("Test", Console.ForegroundColor.RED, Console.BackgroundColor.YELLOW);
        // On systems that don't support ANSI colors, it should just print the text
        assertTrue(outContent.toString().contains("Test"));
    }
    
    @Test
    public void testWritelnWithBothColors() {
        console.writeln("Test", Console.ForegroundColor.RED, Console.BackgroundColor.YELLOW);
        // On systems that don't support ANSI colors, it should just print the text with newline
        assertTrue(outContent.toString().contains("Test"));
        assertTrue(outContent.toString().contains(System.lineSeparator()));
    }
    
    @Test
    public void testWriteIntWithForegroundColor() {
        console.write(42, Console.ForegroundColor.BLUE);
        assertTrue(outContent.toString().contains("42"));
    }
    
    @Test
    public void testWritelnIntWithForegroundColor() {
        console.writeln(42, Console.ForegroundColor.BLUE);
        assertTrue(outContent.toString().contains("42"));
        assertTrue(outContent.toString().contains(System.lineSeparator()));
    }
    
    @Test
    public void testWriteDoubleWithColors() {
        console.write(3.14, Console.ForegroundColor.GREEN, Console.BackgroundColor.BLACK);
        assertTrue(outContent.toString().contains("3.14"));
    }
    
    @Test
    public void testWriteCharWithColors() {
        console.write('X', Console.ForegroundColor.WHITE, Console.BackgroundColor.RED);
        assertTrue(outContent.toString().contains("X"));
    }
    
    @Test
    public void testWriteBooleanWithColors() {
        console.write(false, Console.ForegroundColor.YELLOW);
        assertTrue(outContent.toString().contains("false"));
    }
    
    @Test
    public void testClearScreen() {
        // clearScreen method calls System.out.print with ANSI escape codes
        console.clearScreen();
        assertTrue(outContent.toString().contains("\033[H\033[2J"));
    }
    
    @Test
    public void testMultipleWrites() {
        console.write("Hello ");
        console.write("World");
        console.writeln("!");
        
        String expected = "Hello World!" + System.lineSeparator();
        assertEquals(expected, outContent.toString());
    }
    
    @Test
    public void testConsoleConstructor() {
        // Test that constructor doesn't throw exceptions
        Console testConsole = new Console();
        assertNotNull(testConsole);
    }
}