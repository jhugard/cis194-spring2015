{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
