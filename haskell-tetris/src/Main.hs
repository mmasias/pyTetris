module Main where

import Tetris.Game
import Tetris.Console
import Tetris.Types (Color(..))
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))

main :: IO ()
main = do
  -- Set up console for immediate input
  hSetBuffering stdin NoBuffering
  
  -- Display welcome message
  clearScreen
  displayLineWithColor Green "=== TETRIS HASKELL ==="
  displayLineWithColor Cyan "Conversión funcional del juego Tetris"
  putStrLn ""
  displayLineWithColor Yellow "Controles:"
  putStrLn "  4 = Mover izquierda"
  putStrLn "  6 = Mover derecha" 
  putStrLn "  7 = Rotar sentido antihorario"
  putStrLn "  9 = Rotar sentido horario"
  putStrLn "  q = Salir del juego"
  putStrLn ""
  
  -- Start the game
  putStr "Presiona Enter para comenzar..."
  _ <- getLine
  startStandardGame