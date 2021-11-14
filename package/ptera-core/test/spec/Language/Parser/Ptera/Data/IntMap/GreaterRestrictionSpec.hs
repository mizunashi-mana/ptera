module Language.Parser.Ptera.Data.IntMap.GreaterRestrictionSpec (spec) where

import           Language.Parser.Ptera.Data.IntMap.GreaterRestriction
import           Language.Parser.Ptera.Prelude
import           Test.Hspec

import qualified Data.IntMap as IntMap

spec :: Spec
spec = do
    describe "restrictGreater" do
        it "restricts greater than" do
            restrictGreater 10 IntMap.empty `shouldBe` IntMap.empty @Int
            restrictGreater 10
                do IntMap.singleton 10 10
                `shouldBe` IntMap.empty @Int
            restrictGreater 10
                do IntMap.singleton 11 11
                `shouldBe` IntMap.singleton @Int 11 11
            restrictGreater 10
                do IntMap.fromList [(9,9),(10,10),(11,11)]
                `shouldBe` IntMap.fromList @Int [(11,11)]

