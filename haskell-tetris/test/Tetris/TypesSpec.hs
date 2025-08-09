module Tetris.TypesSpec (spec) where

import Test.Hspec
import Tetris.Types

spec :: Spec
spec = do
  describe "Position" $ do
    it "adds positions correctly" $ do
      let pos1 = Position 2 3
          pos2 = Position 1 4
          result = addPosition pos1 pos2
      result `shouldBe` Position 3 7

    it "handles negative coordinates" $ do
      let pos1 = Position (-1) 2
          pos2 = Position 3 (-5)
          result = addPosition pos1 pos2
      result `shouldBe` Position 2 (-3)

  describe "PieceType" $ do
    it "has all expected piece types" $ do
      [IPiece, OPiece, TPiece, LPiece] `shouldBe` [minBound..maxBound]

  describe "GameCommand" $ do
    it "defines all necessary commands" $ do
      let commands = [Move DLeft, Move DRight, Move DDown, 
                     Rotate Clockwise, Rotate CounterClockwise, 
                     Quit, None]
      length commands `shouldBe` 7