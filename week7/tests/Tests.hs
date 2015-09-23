
module Main where

import Test.Hspec
import HW07

import Prelude hiding (mapM)
import qualified Data.Vector as V
import Control.Monad.Random
import Data.Range.Range

main :: IO ()
main = hspec $ do

    describe "Exercise 1" $ do

      describe "Verify liftM" $
        it "Lifting +1 with Just 5 equals Just 6" $
          liftM (+1) (Just 5) `shouldBe` Just (6 :: Integer)

      describe "Verify swapV" $ do
        it "Swapping two elements swaps the elements" $
          swapV 0 2 (V.fromList [1, 2, 3]) `shouldBe` Just (V.fromList [3::Integer, 2, 1])
        it "Invalid index results in Nothing" $
          swapV 0 2 (V.fromList [1::Integer, 2]) `shouldBe` Nothing
        it "Invalid index results in Nothing" $
          swapV 3 1 (V.fromList [1::Integer, 2]) `shouldBe` Nothing

    describe "Exercise 2" $ do

      describe "Verify mapM" $ do
        it "mapM unit over a list results in unit of list" $
          mapM Just [1..10] `shouldBe` Just [(1::Integer)..10]
        it "mapM with a Nothing in the list results in Nothing" $
          mapM (\a -> if a <= 9 then Just a else Nothing) [(1::Integer)..10] `shouldBe` Nothing

      describe "Verify getElts" $ do
        it "Getting element returns element" $
          getElts [1,3] (V.fromList [0..9]) `shouldBe` Just [1::Integer, 3]
        it "Getting element returns element (and not index)" $
          getElts [1,3] (V.fromList [10..20]) `shouldBe` Just [11::Integer, 13]
        it "Getting non-existent element returns Nothing" $
          getElts [1,99] (V.fromList [(10::Integer)..20]) `shouldBe` Nothing

      describe "Check randomElt" $ do
        it "Random element returns something from the list (not a conclusive test)" $ do
          value <- evalRandIO (randomElt (V.fromList [50]))
          value `shouldBe` Just (50::Integer)
        it "Random element of empty list returns Nothing" $ do
          value <- evalRandIO (randomElt (V.fromList ([] :: [Int])))
          value `shouldBe` Nothing

    describe "Exercise 4" $ do

      describe "Check randomVec" $ do
        it "randomVec produces list of specified length" $ do
          xs <- evalRandIO (randomVec 10 :: Rnd (V.Vector Integer))
          let len = V.length xs
          len `shouldBe` 10
        it "randomVecR produces list of specified length" $ do
          xs <- evalRandIO (randomVec 10 :: Rnd (V.Vector Integer))
          V.length xs `shouldBe` 10
        it "randomVecR produces list of elements in range" $ do
          let len = 100
          let low = 24
          let high = 42
          let range = SpanRange low high
          xs <- evalRandIO (randomVecR len (low,high):: Rnd (V.Vector Integer))
          xs `shouldSatisfy` V.all (inRange range)
          -- let range = V.fromList [low..high]
          -- xs `shouldSatisfy` V.all (`V.elem` range)

      describe "Exercise 5" $ do

        describe "Check that shuffle produces..." $ do
          it "same length list" $ do
            let xs = V.fromList [10..90::Integer]
            sx <- evalRandIO (shuffle xs)
            V.length sx `shouldBe` V.length xs

          it "same elements" $ do
            let xs = V.fromList [10..90::Integer]
            sx <- evalRandIO (shuffle xs)
            V.toList sx `shouldMatchList` V.toList xs

          it "different ordering" $ do
            let xs = V.fromList [10..90::Integer]
            sx <- evalRandIO (shuffle xs)
            V.zip sx xs `shouldSatisfy` V.any (uncurry (/=))
