module Tetris.Pieces where

import Tetris.Types
import System.Random (randomRIO)

-- | Create a new piece of the specified type at the given position
createPiece :: PieceType -> Position -> Piece
createPiece pType pos = Piece
  { pieceType = pType
  , pieceShape = shapeForType pType
  , piecePosition = pos
  }

-- | Get the character symbol for a piece type
pieceSymbol :: PieceType -> Char
pieceSymbol IPiece = 'I'
pieceSymbol OPiece = 'O'
pieceSymbol TPiece = 'T'
pieceSymbol LPiece = 'L'

-- | Define shapes for each piece type
shapeForType :: PieceType -> [[Bool]]
shapeForType IPiece = [[True, True, True, True]]
shapeForType OPiece = [[True, True], [True, True]]
shapeForType TPiece = [[False, True, False], [True, True, True]]
shapeForType LPiece = [[True, False], [True, False], [True, True]]

-- | Move a piece in the specified direction
movePiece :: Direction -> Piece -> Piece
movePiece direction piece =
  let Position x y = piecePosition piece
      newPos = case direction of
        DLeft  -> Position (x - 1) y
        DRight -> Position (x + 1) y
        DDown  -> Position x (y + 1)
  in piece { piecePosition = newPos }

-- | Rotate a piece 90 degrees clockwise
rotateClockwise :: [[Bool]] -> [[Bool]]
rotateClockwise [] = []
rotateClockwise matrix =
  let rows = length matrix
      cols = if null matrix then 0 else length (head matrix)
  in [[matrix !! (rows - 1 - i) !! j | i <- [0..rows-1]] | j <- [0..cols-1]]

-- | Rotate a piece 90 degrees counter-clockwise
rotateCounterClockwise :: [[Bool]] -> [[Bool]]
rotateCounterClockwise matrix = rotateClockwise (rotateClockwise (rotateClockwise matrix))

-- | Rotate a piece in the specified direction
rotatePiece :: RotationDirection -> Piece -> Piece
rotatePiece direction piece =
  let newShape = case direction of
        Clockwise -> rotateClockwise (pieceShape piece)
        CounterClockwise -> rotateCounterClockwise (pieceShape piece)
  in piece { pieceShape = newShape }

-- | Get all possible piece types
allPieceTypes :: [PieceType]
allPieceTypes = [minBound..maxBound]

-- | Generate a random piece type
randomPieceType :: IO PieceType
randomPieceType = do
  index <- randomRIO (0, length allPieceTypes - 1)
  return (allPieceTypes !! index)

-- | Create a random piece at the top center of the board
createRandomPiece :: Int -> IO Piece
createRandomPiece bWidth = do
  pType <- randomPieceType
  let shape = shapeForType pType
      shapeWidth = if null shape then 0 else length (head shape)
      startX = bWidth `div` 2 - (shapeWidth `div` 2)
      startPos = Position startX 0
  return (createPiece pType startPos)

-- | Get the bounding box of a piece (positions occupied by the piece)
getPiecePositions :: Piece -> [Position]
getPiecePositions piece =
  let shape = pieceShape piece
      Position baseX baseY = piecePosition piece
  in [ Position (baseX + x) (baseY + y)
     | (y, row) <- zip [0..] shape
     , (x, cell) <- zip [0..] row
     , cell
     ]