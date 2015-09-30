
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

      describe "Exercise 5" $

        describe "Check that shuffle produces..." $ do
          it "same length list" $ do
            let xs = V.fromList [10..90::Integer]
            sx <- evalRandIO $ shuffle xs
            V.length sx `shouldBe` V.length xs

          it "same elements" $ do
            let xs = V.fromList [10..90::Integer]
            sx <- evalRandIO $ shuffle xs
            V.toList sx `shouldMatchList` V.toList xs

          it "different ordering" $ do
            let xs = V.fromList [10..90::Integer]
            sx <- evalRandIO $ shuffle xs
            V.zip sx xs `shouldSatisfy` V.any (uncurry (/=))

      describe "Exercise 6" $

        describe "Check Partition" $ do

          let sample = V.fromList [ 10,9..1::Integer ]

          it "Partition homework example 1" $
            partitionAt (V.fromList [5::Integer, 2, 8, 3, 6, 1]) 3
            `shouldBe`
            (V.fromList [2, 1], 3, V.fromList [5, 8, 6])

          it "Partition homework example 2" $
            partitionAt (V.fromList [1::Integer, 6, 4, 7, 2, 4]) 2
            `shouldBe`
            (V.fromList [1, 2], 4, V.fromList [6, 7, 4])

          it "Partition at first element" $
            partitionAt sample 0
            `shouldBe` (V.fromList [9,8..1], 10, V.empty)

          it "Partition at last element" $
            partitionAt sample (V.length sample - 1)
            `shouldBe` (V.empty, 1, V.fromList [10,9..2])

      describe "Exercise 7" $ do

        describe "Check qsort" $ do

          it "sorted list" $ do
            let sample = V.fromList [24..42::Integer]
            qsort sample `shouldBe` sample

          it "reverse list" $ do
            let sample = V.fromList [42,41..24::Integer]
            let sorted = V.fromList [24..42]
            qsort sample `shouldBe` sorted

      describe "Exercise 8" $ do

        describe "Check qsortR" $ do

          it "sorted list" $ do
            let sample = V.fromList [24..42::Integer]
            sortedSample <- evalRandIO $ qsortR sample
            sortedSample `shouldBe` sample

          it "reverse list" $ do
            let sample = V.fromList [42,41..24::Integer]
            let sorted = V.fromList [24..42]
            sortedSample <- evalRandIO $ qsortR sample
            sortedSample `shouldBe` sorted

      describe "Exercise 9" $ do

        describe "Given a vector with elements from 42 downto 42" $ do
          let testV = V.fromList [42,41..24 :: Integer]

          it "Out of bounds index returns Nothing" $ do
            let lastIx = V.length testV - 1
            outofbounds <- evalRandIO $ select (lastIx + 1) testV
            outofbounds `shouldBe` Nothing

          it "Lowest element returns 42" $ do
            x <- evalRandIO $ select 0 testV
            x `shouldBe` Just 42

          it "Highest element returns 24" $ do
            let lastIx = V.length testV - 1
            x <- evalRandIO $ select lastIx testV
            x `shouldBe` Just 42

          it "10th element (ix=9) returns 33" $ do
            x <- evalRandIO $ select 9 testV
            x `shouldBe` Just 33
