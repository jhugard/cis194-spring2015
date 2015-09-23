{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits
import GHC.Exts

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret originalFN modifiedFN = do
  obs <- BS.readFile originalFN
  mbs <- BS.readFile modifiedFN
  return $ extractSecret obs mbs
  where
    extractSecret o m =
      BS.pack
        $ filter (/=0)
        $ zipWith xor (BS.unpack o) (BS.unpack m)

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key fout = do
  let fin = fout ++ ".enc"
  encrypted <- BS.readFile fin
  BS.writeFile fout $ decryptViaSecret key encrypted
  where
    decryptViaSecret secret bytes =
      BS.pack
        $ zipWith xor (cycle secret') bytes'
      where
        secret' = BS.unpack secret
        bytes'  = BS.unpack bytes

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
  bytes <- BS.readFile path
  return $ decode bytes

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsPath transactionsPath = do
  mBadTIds <- parseFile victimsPath
  mTs      <- parseFile transactionsPath
  case (mBadTIds, mTs) of
    (Just badTIds, Just ts) ->
      return $ Just $ filterTs badTIds ts
    _ ->
      return Nothing
  where
    filterTs ids = filter (\tx -> tid tx `elem` ids)

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow =
  foldr addTx Map.empty
  where
    addTx trans balMap =
      upd (from trans) (- amount trans) $
        upd (to trans) (amount trans) balMap
      where
        upd name amt m =
          let newBal = case Map.lookup name m of
                      Just bal -> bal + amt
                      Nothing  ->       amt
          in
            Map.insert name newBal m

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal balanceMap =
  let kvs    = Map.toList balanceMap
      kvs'   = sortWith (\(_,v) -> - v) kvs
      (k,_)  = head kvs'
  in
    k

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flow =
    calcReversals [] payers payees
    where
      calcReversals acc [] [] _  = acc
      calcReversals _   [] _  _  = error "Transactions don't balance"
      calcReversals _   _  [] _  = error "Transactions don't balance"
      calcReversals _   _  _  [] = error "Not enough transaction IDs"
      calcReversals acc (f:froms) (t:tos) (i:tids) =
        let reversal = min (amount f) (amount t)
        in
          []

      payers = sortWith byVal . Map.toList $ fst $ Map.partition (<0) flow
      payees = sortWith byVal . Map.toList $ fst $ Map.partition (>0) flow
      byVal (_,v) = - v


-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
