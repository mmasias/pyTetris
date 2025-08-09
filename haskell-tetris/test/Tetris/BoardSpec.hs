module Tetris.BoardSpec (spec) where

import Test.Hspec
import Tetris.Types
import Tetris.Pieces
import Tetris.Board

spec :: Spec
spec = do
  describe "createBoard" $ do
    it "creates a board with correct dimensions" $ do
      let board = createBoard 10 20
      boardWidth board `shouldBe` 10
      boardHeight board `shouldBe` 20
      length (boardGrid board) `shouldBe` 20
      all (\row -> length row == 10) (boardGrid board) `shouldBe` True

    it "creates an empty board" $ do
      let board = createBoard 5 5
          allEmpty = all (all (== '.')) (boardGrid board)
      allEmpty `shouldBe` True

  describe "isValidPosition" $ do
    let board = createBoard 10 20

    it "returns True for valid positions" $ do
      isValidPosition board (Position 0 0) `shouldBe` True
      isValidPosition board (Position 9 19) `shouldBe` True
      isValidPosition board (Position 5 10) `shouldBe` True

    it "returns False for invalid positions" $ do
      isValidPosition board (Position (-1) 0) `shouldBe` False
      isValidPosition board (Position 0 (-1)) `shouldBe` False
      isValidPosition board (Position 10 0) `shouldBe` False
      isValidPosition board (Position 0 20) `shouldBe` False

  describe "isEmptyPosition" $ do
    let board = createBoard 10 20

    it "returns True for empty positions" $ do
      isEmptyPosition board (Position 0 0) `shouldBe` True
      isEmptyPosition board (Position 5 10) `shouldBe` True

    it "returns False for invalid positions" $ do
      isEmptyPosition board (Position (-1) 0) `shouldBe` False
      isEmptyPosition board (Position 10 0) `shouldBe` False

  describe "canPlacePiece" $ do
    let board = createBoard 10 20

    it "allows placing piece in valid empty space" $ do
      let piece = createPiece IPiece (Position 3 5)
      canPlacePiece board piece `shouldBe` True

    it "prevents placing piece out of bounds" $ do
      let piece = createPiece IPiece (Position 8 5)  -- I-piece would extend beyond right edge
      canPlacePiece board piece `shouldBe` False

  describe "clearCompleteLines" $ do
    it "clears complete lines correctly" $ do
      let board = createBoard 3 3
          -- Create a board with one complete line
          grid = [['.','.','.']]
                ++ [['X','X','X']]  -- Complete line
                ++ [['.','.','.']]
          boardWithLine = board { boardGrid = grid }
          (newBoard, linesCleared) = clearCompleteLines boardWithLine
      
      linesCleared `shouldBe` 1
      boardGrid newBoard `shouldBe` [['.','.','.'],['.','.','.'],['.','.','.']]