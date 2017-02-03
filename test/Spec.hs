module Main where


import Test.Hspec
import Compiler


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec =
  describe "TypeChecker" $
    it "type checks" $
      (typeCheck $ Program [] []) `shouldBe` Nothing
