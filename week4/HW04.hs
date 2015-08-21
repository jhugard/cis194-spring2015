{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

normalizeTermsRev :: (Num a, Eq a) => [a] -> [a]
normalizeTermsRev =
      dropWhile (\v -> v == (v - v)) . reverse

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P xs) (P ys) =
      normalizeTermsRev xs == normalizeTermsRev ys

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P terms)  =
      let terms' = normalizeTermsRev terms in
      let sterms' = showPoly terms' (length terms' - 1) in
      intercalate " + " sterms'
      where
          showPoly [] _     = ["0"]
          showPoly [h] _    = [show h]
          showPoly (h:tl) d -- d == degree
            | h == (h-h) = showPoly tl (d - 1)
            | d == 1 = (showTerm h ++ "x") : showPoly tl 0
            | otherwise = (showTerm h ++ "x^" ++ show d) : showPoly tl (d-1)
          showTerm t
            | t == (t-t)  = "" -- term == 0; should never happen
            | t == (t*t)  = "" -- term == 1
            | otherwise   = show t

-- Exercise 4 -----------------------------------------

zipWithTail :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithTail f (a:ta) (b:tb) = f a b : zipWithTail f ta tb
zipWithTail _ a [] = a
zipWithTail _ [] b = b

plus :: (Num a) => Poly a -> Poly a -> Poly a
plus (P as) (P bs) =
  P (zipWithTail (+) as bs)

-- Exercise 5 -----------------------------------------

times :: (Num a) => Poly a -> Poly a -> Poly a
times (P as) (P bs) =
    P (sumPolys (mulTerms as 0))
  where
    sumPolys [] = []
    sumPolys ps = foldr1 (zipWithTail (+)) ps

    mulTerms [] _ = [[]]
    mulTerms (h:tl) degree =
        map (*h) (replicate degree 0 ++ bs) : mulTerms tl (degree+1)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
  (+) = plus
  (*) = times
  negate      = times (P [-1])
  fromInteger i = P [fromInteger i]
  -- No meaningful definitions exist
  abs    = undefined
  signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P p) v =
    sum (zipWith (*) p (map (v^) [0..]))

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined
