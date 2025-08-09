module Tetris.GameSpec (spec) where

import Test.Hspec
import Tetris.Types
import Tetris.Game
import Tetris.Utils

spec :: Spec
spec = do
  describe "calculateScore" $ do
    it "calculates score correctly for single line" $ do
      calculateScore 0 1 `shouldBe` 100

    it "calculates score correctly for multiple lines" $ do
      calculateScore 200 3 `shouldBe` 500

    it "handles zero lines cleared" $ do
      calculateScore 150 0 `shouldBe` 150

  describe "initialGameState" $ do
    it "creates correct initial state" $ do
      let gameState = initialGameState 10 20
      boardWidth (gameBoard gameState) `shouldBe` 10
      boardHeight (gameBoard gameState) `shouldBe` 20
      currentPiece gameState `shouldBe` Nothing
      gameScore gameState `shouldBe` 0
      isGameRunning gameState `shouldBe` True

  describe "shouldContinueGame" $ do
    it "returns True for running game" $ do
      let gameState = initialGameState 10 20
      shouldContinueGame gameState `shouldBe` True

    it "returns False for ended game" $ do
      let gameState = endGame (initialGameState 10 20)
      shouldContinueGame gameState `shouldBe` False