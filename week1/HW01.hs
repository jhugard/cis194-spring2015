{-# OPTIONS_GHC -Wall #-}
module HW01 where

import Data.Char

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
-- toRevDigits n = reverse $ toDigits n
toRevDigits n
	| n <= 0		= []
 	| otherwise = lastDigit n : toRevDigits (dropLastDigit n)

toDigits :: Integer -> [Integer]
toDigits = reverse . toRevDigits

toDigits' :: Integer -> [Integer]
toDigits' n
	| n <= 0		= []
	| otherwise	= map (toInteger . digitToInt) $ show n


-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] 		= []
doubleEveryOther [a] 		= [a]
doubleEveryOther (a : b : tl) =	a : (b+b) : doubleEveryOther tl

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigits) 0

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n =
		((sumDigits . doubleEveryOther . toRevDigits) n `mod` 10)
		== 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a _ c = [(a,c)]
-- hanoi 2 a b c = [(a,b), (a,c), (b,c)]
hanoi n a b c =
	concat [
		hanoi (n-1) a c b,
		hanoi    1  a b c,
		hanoi (n-1) b a c
		]
