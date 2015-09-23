{-# OPTIONS_GHC -Wall -XBangPatterns #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 :
  zipWith (+) fibs2' (tail fibs2')

fibs2' :: [Integer]
fibs2' = 1 : 1 : remainingFibs 0
  where
    remainingFibs n =
      let a : b : _ = n `drop` fibs2
      in
        a + b : remainingFibs (n+1)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 10 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons h t) =
  h : streamToList t

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons h t) =
      Cons (f h) (fmap f t)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a =
  Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate f seed =
  Cons seed (sIterate f (f seed))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) ys =
  Cons x (sInterleave ys xs)

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons h t) =
  h : sTake (n-1) t

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 1

ruler :: Stream Integer
ruler = ruler' 0
  where
    ruler' n = sInterleave (sRepeat n) (ruler' (n+1))

{-
 ruler' =
  sInterleave
    (sRepeat 0)
    (sInterleave
      (sRepeat 1)
      (sInterleave
        (sRepeat 2)
        (sIterate (+1) 3)))

checkRuler :: [(Integer,Integer,Integer)]
checkRuler =
  zipWith
    (\a b -> (a, b, a `mod` (2^b)))
    [1..]
    (streamToList ruler)

-}


-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand =
    sIterate next . next
    where
      next x = (1103515245 * x + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ~237 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ~1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax xs =
  Just $ foldl' (\ (!a,!z) !x -> (x `min` a, x `max` z)) (2147483648,0) xs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
