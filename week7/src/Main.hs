{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import HW07
import Control.Monad.Random

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
