module Tetris.Utils where

import Tetris.Types

-- | Calculate score based on lines cleared
calculateScore :: Int -> Int -> Int
calculateScore currentScore linesCleared = currentScore + (linesCleared * 100)

-- | Check if the game should continue
shouldContinueGame :: GameState -> Bool
shouldContinueGame gameState = isGameRunning gameState

-- | Create initial game state
initialGameState :: Int -> Int -> GameState
initialGameState width height = GameState
  { gameBoard = Board width height (replicate height (replicate width '.'))
  , currentPiece = Nothing
  , gameScore = 0
  , isGameRunning = True
  }

-- | Update game state with a new piece
setCurrentPiece :: GameState -> Piece -> GameState
setCurrentPiece gameState piece = gameState { currentPiece = Just piece }

-- | Update game score
updateScore :: GameState -> Int -> GameState
updateScore gameState linesCleared = 
  gameState { gameScore = calculateScore (gameScore gameState) linesCleared }

-- | End the game
endGame :: GameState -> GameState
endGame gameState = gameState { isGameRunning = False }

-- | Update the board in game state
updateBoard :: GameState -> Board -> GameState
updateBoard gameState newBoard = gameState { gameBoard = newBoard }

-- | Safe list indexing
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n
  | n < 0 = Nothing
  | otherwise = safeIndex xs (n - 1)

-- | Safe 2D list indexing
safeIndex2D :: [[a]] -> Int -> Int -> Maybe a
safeIndex2D grid row col = do
  rowData <- safeIndex grid row
  safeIndex rowData col