{-# OPTIONS_GHC -Wall #-}
module HW02 where


-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Utilities ------------------------------------------

-- Shorthand for if b then x else y
bool :: a -> a -> Bool -> a
bool t _ True = t
bool _ f False = f

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the secret code and the guess
exactMatches :: Code -> Code -> Int
exactMatches secret guess =
  sum . map boolToInt $ zipWith (==) secret guess
  where
      boolToInt True  = 1;
      boolToInt False = 0

-- Exercise 2 -----------------------------------------

-- For each color, count how many times it occurs in xs
countColors :: Code -> [Int]
countColors xs =
    foldr (\color acc -> count color : acc) [] colors
    where
      count color =
        length $ filter (==color) xs;

countColors' :: Code -> [Int]
countColors' xs =
    foldr (\color acc -> count color : acc) [] colors
    where
      count color =
        foldr (\other acc -> if other==color then acc+1 else acc) 0 xs


-- Count number of matches between the secret code and the guess
matches :: Code -> Code -> Int
matches secret guess =
  sum $ zipWith min (countColors secret) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the secret code
getMove :: Code -> Code -> Move
getMove secret guess =
  Move guess exact (nonExact-exact)
    where exact    = exactMatches secret guess;
          nonExact = matches secret guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact nonExact) test =
  let Move _ testExact testNonExact = getMove test guess in
  testExact == exact && testNonExact == nonExact

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes n =
  sequence $ replicate n colors
--  Control.Monad.replacteM n colors

-- Exercise 7 -----------------------------------------

-- Start with a guess
-- Get the Move
-- Check to see if we have a match
-- If not,
--  Filter out only consistent codes
--  Recurse
-- Otherwise
--  FTW!

solve :: Code -> [Move]
solve secret =
  reverse $ guesses (allCodes $ length secret) []
  where
    guesses [] acc = acc;
    guesses (code:codespace) acc =
      let move = getMove secret code in
      let codespace' = filter (isConsistent move) codespace in
      guesses codespace' $ move : acc

solve' :: Code -> [Move]
solve' secret =
  let len = length secret in
  let startMove = getMove secret $ replicate len Red in
  reverse $ guesses startMove (allCodes len) []
  where
    guesses move codes acc =
      if isFTW move then
        move : acc
      else
        let codes' = filter (isConsistent move) codes in
        let nextCode : codes'' = codes' in
        let nextMove = getMove secret nextCode in
        guesses nextMove codes'' (move : acc);
    isFTW (Move _ exact _) = exact == length secret

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
