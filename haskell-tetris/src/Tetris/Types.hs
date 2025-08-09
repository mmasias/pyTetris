{-# LANGUAGE DeriveGeneric #-}

module Tetris.Types where

import GHC.Generics (Generic)

-- | Position on the game board
data Position = Position
  { posX :: Int
  , posY :: Int
  } deriving (Show, Eq, Generic)

-- | Add two positions
addPosition :: Position -> Position -> Position
addPosition (Position x1 y1) (Position x2 y2) = Position (x1 + x2) (y1 + y2)

-- | Piece type enumeration
data PieceType = IPiece | OPiece | TPiece | LPiece
  deriving (Show, Eq, Enum, Bounded, Generic)

-- | Tetris piece with shape and position
data Piece = Piece
  { pieceType :: PieceType
  , pieceShape :: [[Bool]]  -- True represents filled cell
  , piecePosition :: Position
  } deriving (Show, Eq, Generic)

-- | Game board representation
data Board = Board
  { boardWidth :: Int
  , boardHeight :: Int
  , boardGrid :: [[Char]]  -- '.' for empty, character for filled
  } deriving (Show, Eq, Generic)

-- | Game state
data GameState = GameState
  { gameBoard :: Board
  , currentPiece :: Maybe Piece
  , gameScore :: Int
  , isGameRunning :: Bool
  } deriving (Show, Eq, Generic)

-- | Movement direction
data Direction = DLeft | DRight | DDown
  deriving (Show, Eq, Generic)

-- | Rotation direction
data RotationDirection = Clockwise | CounterClockwise
  deriving (Show, Eq, Generic)

-- | Game command from user input
data GameCommand
  = Move Direction
  | Rotate RotationDirection
  | Quit
  | None
  deriving (Show, Eq, Generic)

-- | Console colors for output
data Color
  = Black | Red | Green | Yellow | Blue | Purple | Cyan | White
  deriving (Show, Eq, Enum, Bounded, Generic)