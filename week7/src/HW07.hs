{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
--import Data.Functor
import Data.Monoid
import Data.Vector (Vector, (!), (!?), (//))

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma =
  ma >>= return . f -- aka, (\a -> return $ f a)

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV ix iy v =
  liftM2 swap (v !? ix) (v !? iy)
  where
    swap a b =
      v // [(iy,a), (ix,b)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f
{- by hand
mapM f = myseq . map f
  where
    myseq =
      foldr k (return [])
      where
        k mx macc = do
          x <- mx
          xs <- macc
          return (x:xs)
-}
{- alternative
        k mx macc =
            mx >>= (\x ->
              macc >>= (\xs ->
                return (x:xs))
            )
-}

getElts :: [Int] -> Vector a -> Maybe [a]
getElts ixs v = mapM (v !?) ixs

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = do
  let len = V.length v
  rndIx <- getRandomR (0,len - 1)
  return $ v !? rndIx

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n =
  V.generateM n $ const getRandom
{-
randomVec = gen
  where
    gen :: Random a => Int -> Rnd (Vector a)
    gen 0 = return V.empty
    gen n = do
      r <- getRandom
      rs <- gen (n-1)
      return $ r `V.cons` rs
-}

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n (a,z)=
  V.generateM n $ const $ getRandomR (a,z)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v =
  shuf (return v) (V.length v - 1)
  where
    shuf mv 0 = mv
    shuf mv ix = do
      v' <- mv
      iy <- getRandomR (0,ix)
      let v'' = unsafeSwapV v' ix iy
      shuf (return v'') (ix-1)
    unsafeSwapV xs x y =
      xs // [(x, xs ! y), (y, xs ! x)]

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v ixPivot =
  (left, pivot, right)
  where
    end = V.length v - 1
    ixs = [0..(ixPivot-1)] ++ [(ixPivot+1)..end]
    pivot = v ! ixPivot
    left  = V.fromList [ l | ix <- ixs, let l = v ! ix, l < pivot ]
    right = V.fromList [ r | ix <- ixs, let r = v ! ix, r >= pivot ]


-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort = undefined


-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR = undefined

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty
