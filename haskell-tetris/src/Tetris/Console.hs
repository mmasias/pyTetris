{-# LANGUAGE ScopedTypeVariables #-}

module Tetris.Console where

import Tetris.Types (GameCommand(..), Direction(..), RotationDirection(..), Color(..))
import qualified System.Console.ANSI as ANSI
import System.IO (hFlush, stdout)
import Control.Exception (catch, SomeException)

-- | Clear the entire screen
clearScreen :: IO ()
clearScreen = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0

-- | Display the game board with borders
displayBoard :: [[Char]] -> IO ()
displayBoard grid = do
  let width = length (head grid)
      topBorder = "<!" ++ replicate (width * 2 + 1) '=' ++ "!>"
      bottomBorder = "<!" ++ replicate (width * 2 + 1) '=' ++ "!>"
      bottomLine = concat (replicate (width + 4) "\\/")
  
  putStrLn topBorder
  mapM_ displayRow grid
  putStrLn bottomBorder
  putStrLn bottomLine
  where
    displayRow :: [Char] -> IO ()
    displayRow row = do
      putStr "<!  "
      mapM_ (\c -> putStr [c, ' ']) row
      putStrLn " !>"

-- | Display text with color
displayWithColor :: Color -> String -> IO ()
displayWithColor color text = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (ansiColor color)]
  putStr text
  ANSI.setSGR [ANSI.Reset]
  where
    ansiColor :: Color -> ANSI.Color
    ansiColor Black  = ANSI.Black
    ansiColor Red    = ANSI.Red
    ansiColor Green  = ANSI.Green
    ansiColor Yellow = ANSI.Yellow
    ansiColor Blue   = ANSI.Blue
    ansiColor Purple = ANSI.Magenta
    ansiColor Cyan   = ANSI.Cyan
    ansiColor White  = ANSI.White

-- | Display text with color and newline
displayLineWithColor :: Color -> String -> IO ()
displayLineWithColor color text = do
  displayWithColor color text
  putStrLn ""

-- | Display game score
displayScore :: Int -> IO ()
displayScore score = do
  displayWithColor Cyan "Score: "
  putStrLn (show score)

-- | Display game instructions
displayInstructions :: IO ()
displayInstructions = do
  putStrLn ""
  displayWithColor Yellow "Controles:"
  putStrLn " 4=izquierda, 6=derecha, 7=rotar↺, 9=rotar↻, q=salir"

-- | Read a single character from input
readChar :: IO Char
readChar = do
  putStr "Comando: "
  hFlush stdout
  input <- getLine
  return $ if null input then ' ' else head input

-- | Parse user input into game command
parseCommand :: Char -> GameCommand
parseCommand '4' = Move DLeft
parseCommand '6' = Move DRight
parseCommand '7' = Rotate CounterClockwise
parseCommand '9' = Rotate Clockwise
parseCommand 'q' = Quit
parseCommand 'Q' = Quit
parseCommand _   = None

-- | Read and parse a game command
readGameCommand :: IO GameCommand
readGameCommand = do
  char <- readChar
  return (parseCommand char)

-- | Display a message with highlighting
displayMessage :: String -> IO ()
displayMessage msg = do
  displayLineWithColor Green msg
  putStr "Presiona Enter para continuar..."
  hFlush stdout
  _ <- getLine
  return ()

-- | Display game over message
displayGameOver :: Int -> IO ()
displayGameOver finalScore = do
  putStrLn ""
  displayLineWithColor Red "¡GAME OVER!"
  displayWithColor Cyan "Puntuación final: "
  putStrLn (show finalScore)
  putStrLn ""

-- | Display lines cleared message
displayLinesCleared :: Int -> IO ()
displayLinesCleared count = do
  let message = "¡" ++ show count ++ " línea(s) eliminada(s)!"
  displayMessage message

-- | Safe console operations with error handling
safeConsoleOp :: IO a -> IO a -> IO a
safeConsoleOp operation fallback = 
  catch operation (\(_ :: SomeException) -> fallback)

-- | Display the complete game state
displayGameState :: [[Char]] -> Int -> IO ()
displayGameState grid score = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  displayBoard grid
  displayScore score
  displayInstructions