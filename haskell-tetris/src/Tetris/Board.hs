module Tetris.Board where

import Tetris.Types
import Tetris.Pieces

-- | Create a new empty board
createBoard :: Int -> Int -> Board
createBoard width height = Board
  { boardWidth = width
  , boardHeight = height
  , boardGrid = replicate height (replicate width '.')
  }

-- | Check if a position is within board bounds
isValidPosition :: Board -> Position -> Bool
isValidPosition board (Position x y) =
  x >= 0 && x < boardWidth board && y >= 0 && y < boardHeight board

-- | Check if a position on the board is empty
isEmptyPosition :: Board -> Position -> Bool
isEmptyPosition board pos@(Position x y)
  | not (isValidPosition board pos) = False
  | otherwise = (boardGrid board !! y !! x) == '.'

-- | Check if a piece can be placed at its current position
canPlacePiece :: Board -> Piece -> Bool
canPlacePiece board piece =
  all (isEmptyPosition board) (getPiecePositions piece)

-- | Check if a piece can move in the specified direction
canMovePiece :: Board -> Piece -> Direction -> Bool
canMovePiece board piece direction =
  canPlacePiece board (movePiece direction piece)

-- | Check if a piece can be rotated
canRotatePiece :: Board -> Piece -> RotationDirection -> Bool
canRotatePiece board piece direction =
  canPlacePiece board (rotatePiece direction piece)

-- | Place a piece permanently on the board
placePiece :: Board -> Piece -> Board
placePiece board piece =
  let positions = getPiecePositions piece
      symbol = pieceSymbol (pieceType piece)
      newGrid = foldl (updateGrid symbol) (boardGrid board) positions
  in board { boardGrid = newGrid }
  where
    updateGrid :: Char -> [[Char]] -> Position -> [[Char]]
    updateGrid symbol grid (Position x y)
      | y < 0 || y >= length grid = grid  -- Invalid y coordinate
      | null grid = grid  -- Empty grid
      | x < 0 || x >= length (head grid) = grid  -- Invalid x coordinate
      | otherwise = 
          case splitAt y grid of
            (beforeRows, []) -> grid  -- y is out of bounds
            (beforeRows, targetRow:afterRows) ->
              case splitAt x targetRow of
                (beforeCols, []) -> grid  -- x is out of bounds
                (beforeCols, _:afterCols) ->
                  let newRow = beforeCols ++ [symbol] ++ afterCols
                  in beforeRows ++ [newRow] ++ afterRows

-- | Check if a line is complete (all positions filled)
isCompleteLine :: [Char] -> Bool
isCompleteLine line = all (/= '.') line

-- | Clear all complete lines and return (new board, lines cleared)
clearCompleteLines :: Board -> (Board, Int)
clearCompleteLines board =
  let grid = boardGrid board
      (incompleteLines, completedCount) = foldl processLine ([], 0) grid
      emptyLines = replicate completedCount (replicate (boardWidth board) '.')
      newGrid = emptyLines ++ incompleteLines
  in (board { boardGrid = newGrid }, completedCount)
  where
    processLine :: ([[Char]], Int) -> [Char] -> ([[Char]], Int)
    processLine (acc, count) line
      | isCompleteLine line = (acc, count + 1)
      | otherwise = (acc ++ [line], count)

-- | Get a copy of the board with the current piece rendered on it
renderBoardWithPiece :: Board -> Maybe Piece -> [[Char]]
renderBoardWithPiece board Nothing = boardGrid board
renderBoardWithPiece board (Just piece) =
  let positions = getPiecePositions piece
      symbol = pieceSymbol (pieceType piece)
      baseGrid = boardGrid board
  in foldl (updateGridDisplay symbol) baseGrid positions
  where
    updateGridDisplay :: Char -> [[Char]] -> Position -> [[Char]]
    updateGridDisplay symbol grid (Position x y)
      | y < 0 || y >= length grid = grid
      | null grid = grid
      | x < 0 || x >= length (head grid) = grid
      | otherwise =
          case splitAt y grid of
            (beforeRows, []) -> grid
            (beforeRows, targetRow:afterRows) ->
              case splitAt x targetRow of
                (beforeCols, []) -> grid
                (beforeCols, _:afterCols) ->
                  let newRow = beforeCols ++ [symbol] ++ afterCols
                  in beforeRows ++ [newRow] ++ afterRows

-- | Check if the game is over (new piece cannot be placed)
isGameOver :: Board -> Piece -> Bool
isGameOver board piece = not (canPlacePiece board piece)