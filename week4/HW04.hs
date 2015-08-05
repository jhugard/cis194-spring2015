{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1]

-- Exercise 2 ----------------------------------------

normalizeTermsRev :: (Num a, Eq a) => [a] -> [a]
normalizeTermsRev =
      dropWhile (\v -> v == (v - v)) . reverse

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P xs) (P ys) =
      normalizeTermsRev xs == normalizeTermsRev ys

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Enum a, Show a) => Show (Poly a) where
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
            | t == (t-t)      = "" -- term == 0; should never happen
            | t == succ (t-t) = "" -- term == 1
            | otherwise       = show t

-- Exercise 4 -----------------------------------------

zipWithTail :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithTail f (a:ta) (b:tb) = f a b : zipWithTail f ta tb
zipWithTail _ a [] = a
zipWithTail _ [] b = b

plus :: (Num a) => Poly a -> Poly a -> Poly a
plus (P as) (P bs) =
  P (zipWithTail (+) as bs)

-- Exercise 5 -----------------------------------------

times :: (Num a, Enum a) => Poly a -> Poly a -> Poly a
times (P as) (P bs) =
  P (sumPolys (mulTerms as bs 0))
  where
    sumPolys [] = []
    sumPolys ps = foldr1 (zipWithTail (+)) ps

    mulTerms :: (Num t, Enum t) => [t] -> [t] -> Int -> [[t]]
    mulTerms [] bs degree = [[]]
    mulTerms (h:tl) bs degree =
      let zero = (h-h)
      in replicate degree zero
          ++ map (\b -> h * b) bs
          : mulTerms tl bs (degree+1)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined
