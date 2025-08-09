module Tetris.PiecesSpec (spec) where

import Test.Hspec
import Tetris.Types
import Tetris.Pieces

spec :: Spec
spec = do
  describe "createPiece" $ do
    it "creates a piece with correct type and position" $ do
      let piece = createPiece IPiece (Position 5 10)
      pieceType piece `shouldBe` IPiece
      piecePosition piece `shouldBe` Position 5 10

  describe "pieceSymbol" $ do
    it "returns correct symbols for each piece type" $ do
      pieceSymbol IPiece `shouldBe` 'I'
      pieceSymbol OPiece `shouldBe` 'O'
      pieceSymbol TPiece `shouldBe` 'T'
      pieceSymbol LPiece `shouldBe` 'L'

  describe "shapeForType" $ do
    it "returns correct shape for I piece" $ do
      shapeForType IPiece `shouldBe` [[True, True, True, True]]

    it "returns correct shape for O piece" $ do
      shapeForType OPiece `shouldBe` [[True, True], [True, True]]

    it "returns correct shape for T piece" $ do
      shapeForType TPiece `shouldBe` [[False, True, False], [True, True, True]]

    it "returns correct shape for L piece" $ do
      shapeForType LPiece `shouldBe` [[True, False], [True, False], [True, True]]

  describe "movePiece" $ do
    it "moves piece left correctly" $ do
      let piece = createPiece IPiece (Position 5 10)
          moved = movePiece DLeft piece
      piecePosition moved `shouldBe` Position 4 10

    it "moves piece right correctly" $ do
      let piece = createPiece IPiece (Position 5 10)
          moved = movePiece DRight piece
      piecePosition moved `shouldBe` Position 6 10

    it "moves piece down correctly" $ do
      let piece = createPiece IPiece (Position 5 10)
          moved = movePiece DDown piece
      piecePosition moved `shouldBe` Position 5 11

  describe "rotateClockwise" $ do
    it "rotates a 2x2 matrix correctly" $ do
      let matrix = [[True, False], [False, True]]
          rotated = rotateClockwise matrix
      rotated `shouldBe` [[False, True], [True, False]]

  describe "getPiecePositions" $ do
    it "returns correct positions for I piece" $ do
      let piece = createPiece IPiece (Position 2 3)
          positions = getPiecePositions piece
      positions `shouldBe` [Position 2 3, Position 3 3, Position 4 3, Position 5 3]