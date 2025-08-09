import Test.Hspec

import qualified Tetris.TypesSpec
import qualified Tetris.PiecesSpec
import qualified Tetris.BoardSpec
import qualified Tetris.GameSpec

main :: IO ()
main = hspec $ do
  describe "Tetris.Types" Tetris.TypesSpec.spec
  describe "Tetris.Pieces" Tetris.PiecesSpec.spec
  describe "Tetris.Board" Tetris.BoardSpec.spec
  describe "Tetris.Game" Tetris.GameSpec.spec