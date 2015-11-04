{-# LANGUAGE ScopedTypeVariables #-}

module BinTree.BinTreeSpec where

import BinTree (BinTree (..))
import qualified BinTree

import Data.List (foldl', nub, sort)
import qualified Test.QuickCheck as QC
import Test.Hspec

spec = do
    describe "BinTree.insert" $
        it "insert 1 into an empty tree" $
            BinTree.insert (1 :: Int) Nil `shouldBe` Node 1 Nil Nil

    describe "BinTree.insert" $
        it "insert 2 1 3 4 into an empty tree" $
            BinTree.insert (4 :: Int) (BinTree.insert 1 (BinTree.insert 3 (BinTree.insert 2 Nil)))
              `shouldBe` Node 2 (Node 1 Nil Nil) (Node 3 Nil (Node 4 Nil Nil))

    describe "BinTree.insert" $
        it "make sure duplicates are ignored" $
            BinTree.insert (3 :: Int) (BinTree.insert 1 (BinTree.insert 3 (BinTree.insert 2 Nil)))
              `shouldBe` Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)

    describe "BinTree.inorder" $
        it "make sure an in-order traversal ouputs a sorted list" $
            let propSorted (xs :: [Int]) = BinTree.inorder (foldl' (flip BinTree.insert) Nil xs) == (sort . nub) xs
             in QC.quickCheck propSorted
