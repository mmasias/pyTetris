module Tetris.Game where

import Tetris.Types
import Tetris.Pieces
import Tetris.Board
import Tetris.Console
import Tetris.Utils
import Control.Monad (when)

-- | Main game loop
gameLoop :: GameState -> IO ()
gameLoop gameState = do
  when (shouldContinueGame gameState) $ do
    -- Display current game state
    let grid = case currentPiece gameState of
          Nothing -> boardGrid (gameBoard gameState)
          Just piece -> renderBoardWithPiece (gameBoard gameState) (Just piece)
    displayGameState grid (gameScore gameState)
    
    -- Read user command
    command <- readGameCommand
    
    -- Process command and update game state
    newGameState <- processCommand gameState command
    
    -- Continue game loop
    gameLoop newGameState

-- | Process a game command and update state
processCommand :: GameState -> GameCommand -> IO GameState
processCommand gameState Quit = return (endGame gameState)
processCommand gameState None = processGravity gameState
processCommand gameState (Move direction) = do
  newGameState <- tryMovePiece gameState direction
  processGravity newGameState
processCommand gameState (Rotate direction) = do
  newGameState <- tryRotatePiece gameState direction
  processGravity newGameState

-- | Try to move the current piece
tryMovePiece :: GameState -> Direction -> IO GameState
tryMovePiece gameState direction =
  case currentPiece gameState of
    Nothing -> return gameState
    Just piece ->
      if canMovePiece (gameBoard gameState) piece direction
        then return gameState { currentPiece = Just (movePiece direction piece) }
        else return gameState

-- | Try to rotate the current piece
tryRotatePiece :: GameState -> RotationDirection -> IO GameState
tryRotatePiece gameState direction =
  case currentPiece gameState of
    Nothing -> return gameState
    Just piece ->
      if canRotatePiece (gameBoard gameState) piece direction
        then return gameState { currentPiece = Just (rotatePiece direction piece) }
        else return gameState

-- | Apply gravity (move piece down or place it)
processGravity :: GameState -> IO GameState
processGravity gameState =
  case currentPiece gameState of
    Nothing -> spawnNewPiece gameState
    Just piece ->
      if canMovePiece (gameBoard gameState) piece DDown
        then return gameState { currentPiece = Just (movePiece DDown piece) }
        else placePieceAndContinue gameState piece

-- | Place the current piece and continue the game
placePieceAndContinue :: GameState -> Piece -> IO GameState
placePieceAndContinue gameState piece = do
  -- Place piece on board
  let newBoard = placePiece (gameBoard gameState) piece
      gameState' = updateBoard gameState newBoard
  
  -- Clear complete lines
  let (finalBoard, linesCleared) = clearCompleteLines newBoard
      gameState'' = updateBoard gameState' finalBoard
      gameState''' = updateScore gameState'' linesCleared
  
  -- Show lines cleared message if any
  when (linesCleared > 0) $ displayLinesCleared linesCleared
  
  -- Spawn new piece
  spawnNewPiece gameState'''

-- | Spawn a new piece
spawnNewPiece :: GameState -> IO GameState
spawnNewPiece gameState = do
  newPiece <- createRandomPiece (boardWidth (gameBoard gameState))
  
  -- Check if game is over
  if isGameOver (gameBoard gameState) newPiece
    then do
      displayGameOver (gameScore gameState)
      return (endGame gameState)
    else return (setCurrentPiece gameState newPiece)

-- | Initialize and start a new game
startGame :: Int -> Int -> IO ()
startGame width height = do
  let initialState = initialGameState width height
  gameStateWithPiece <- spawnNewPiece initialState
  gameLoop gameStateWithPiece

-- | Start a standard Tetris game (10x20 board)
startStandardGame :: IO ()
startStandardGame = startGame 10 20