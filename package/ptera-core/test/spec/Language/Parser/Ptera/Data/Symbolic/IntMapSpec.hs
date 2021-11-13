module Language.Parser.Ptera.Data.Symbolic.IntMapSpec (spec) where

import           Language.Parser.Ptera.Data.Symbolic.IntMap
import           Language.Parser.Ptera.Prelude              hiding (empty,
                                                             lookup)
import           Test.Hspec

import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet


spec :: Spec
spec = do
    describe "equal" do
        it "returns true if givens are same" do
            single10To100 == single10To100 `shouldBe` True
            full100 == full100 `shouldBe` True
            threeId == threeId `shouldBe` True
            negative100 == negative100 `shouldBe` True

        it "returns false if givens are not equal" do
            single10To100 == full100 `shouldBe` False
            full100 == negative100 `shouldBe` False
            threeId == single10To100 `shouldBe` False
            full100 == full 10 `shouldBe` False
            single10To100 == singleton 10 101 `shouldBe` False

        it "returns true if givens are equal" do
            let deletedAndInsertedFull100 = insert 13 100 do delete 13 full100
            full100 == deletedAndInsertedFull100 `shouldBe` True

    describe "lookup" do
        it "returns an element if available" do
            lookup 10 single10To100 `shouldBe` Just 100
            lookup 10 full100 `shouldBe` Just 100
            lookup 11 threeId `shouldBe` Just 11
            lookup 11 negative100 `shouldBe` Just 100

        it "returns nothing if not available" do
            lookup 11 (empty @Int) `shouldBe` Nothing
            lookup 11 single10To100 `shouldBe` Nothing
            lookup 1 threeId `shouldBe` Nothing
            lookup 13 negative100 `shouldBe` Nothing

    describe "keys" do
        it "returns a set of keys" do
            keys (empty @Int) `shouldBe` mempty
            keys single10To100 `shouldBe` SymbolicIntSet.singleton 10
            keys full100 `shouldBe` SymbolicIntSet.full
            keys threeId `shouldBe` SymbolicIntSet.fromList [11, 101, 13]
            keys negative100 `shouldBe` do SymbolicIntSet.invert do SymbolicIntSet.fromList [13, 15]

    describe "alterBulk" do
        it "returns original map without any keys" do
            alterBulk
                do \_ -> Just 10
                mempty
                full100
                `shouldBe` full100
            alterBulk
                do \_ -> Nothing
                mempty
                negative100
                `shouldBe` negative100

        it "returns altered map with full keys" do
            alterBulk
                do \_ -> Just 10
                SymbolicIntSet.full
                negative100
                `shouldBe` full 10
            alterBulk
                do \_ -> Nothing
                SymbolicIntSet.full
                threeId
                `shouldBe` empty
            alterBulk
                do \_ -> Nothing
                SymbolicIntSet.full
                full100
                `shouldBe` empty
            alterBulk
                do \_ -> Just 10
                SymbolicIntSet.full
                do empty @Int
                `shouldBe` full 10
            alterBulk
                do \m -> (10 +) <$> m
                SymbolicIntSet.full
                full100
                `shouldBe` full 110

        it "returns altered map with straight keys" do
            alterBulk
                do \_ -> Just 101
                do SymbolicIntSet.singleton 10
                negative100
                `shouldBe` do insert 10 101 negative100
            alterBulk
                do \_ -> Nothing
                do SymbolicIntSet.singleton 10
                full100
                `shouldBe` do delete 10 full100
            alterBulk
                do \_ -> Just 101
                do SymbolicIntSet.singleton 13
                negative100
                `shouldBe` do insert 13 101 negative100
            alterBulk
                do \m -> (10 +) <$> m
                do SymbolicIntSet.singleton 10
                full100
                `shouldBe` do insert 10 110 full100

        it "returns altered map with negative keys" do
            alterBulk
                do \_ -> Just 101
                do SymbolicIntSet.invert do SymbolicIntSet.singleton 10
                negative100
                `shouldBe` do insert 10 101 do full 101
            alterBulk
                do \_ -> Just 101
                do SymbolicIntSet.singleton 10
                full100
                `shouldBe` do insert 10 101 do full100

    describe "merge" do
        it "merges two maps" do
            merge
                do \x1 x2 -> Just do x1 + x2
                do \x -> Just do x + 200
                do \x -> Just do x + 300
                do full @Int 13
                do full @Int 14
                `shouldBe` full @Int 27
            merge
                do \x1 x2 -> Just do x1 + x2
                do \x -> Just do x + 200
                do \x -> Just do x + 300
                do singleton @Int 13 13
                do singleton @Int 14 14
                `shouldBe` do insert 13 213 do insert 14 314 do empty @Int
            merge
                do \x1 x2 -> Just do x1 + x2
                do \x -> Just do x + 200
                do \x -> Just do x + 300
                do empty @Int
                do empty @Int
                `shouldBe` do empty @Int
            merge
                do \x1 x2 -> Just do x1 + x2
                do \x -> Just do x + 200
                do \x -> Just do x + 300
                do singleton @Int 13 13
                do full @Int 14
                `shouldBe` insert 13 27 do full @Int 314

single10To100 :: IntMap Int
single10To100 = singleton 10 100

full100 :: IntMap Int
full100 = full 100

threeId :: IntMap Int
threeId = insert 11 11 do insert 101 101 do insert 13 13 do empty

negative100 :: IntMap Int
negative100 = delete 13 do delete 15 do full100
