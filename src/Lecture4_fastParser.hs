{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Lecture4_fastParser
  ( -- * Main running function
    main,

    -- * Types
    TradeType (..),
    Row (..),
    MaxLen (..),
    Stats (..),

    -- * Internal functions
    parseRow,
    rowToStats,
    displayStats,
    printProductStats,
  )
where

import Data.Foldable (Foldable (foldl'))
import Data.Semigroup (Max (..), Min (..), Sum (..))
import GHC.IO.IOMode (IOMode (ReadMode))
import System.Environment (getArgs)
import System.IO (hGetContents, openFile)
import Text.Read (readMaybe)

data TradeType
  = Buy
  | Sell
  deriving (Show, Eq, Read)

data Row = Row
  { rowProduct :: String,
    rowTradeType :: TradeType,
    rowCost :: Int
  }
  deriving (Show, Eq)

splitBy :: Char -> String -> [String]
splitBy delim = foldr f []
  where
    f c []
      | c == delim = []
      | otherwise = [[c]]
    f c acc@(x : xs)
      | c == delim = "" : acc
      | otherwise = (c : x) : xs

parseRow :: String -> Maybe Row
parseRow s =
  pname (splitBy ',' s) >>= ptype >>= pprice
  where
    pname :: [String] -> Maybe ([String], String)
    pname [] = Nothing
    pname (x : xs) = if null x then Nothing else Just (xs, x)

    ptype :: ([String], String) -> Maybe ([String], (String, TradeType))
    ptype (x : xs, st) = readMaybe x >>= \tt -> Just (xs, (st, tt))
    ptype _ = Nothing

    pprice :: ([String], (String, TradeType)) -> Maybe Row
    pprice (x : xs, (st, t)) =
      if null xs
        then
          readMaybe x >>= \n -> if n < 0 then Nothing else Just (Row st t n)
        else Nothing
    pprice _ = Nothing

newtype MaxLen = MaxLen
  { unMaxLen :: String
  }
  deriving (Show, Eq)

instance Semigroup MaxLen where
  (<>) (MaxLen a) (MaxLen b) = if length a >= length b then MaxLen a else MaxLen b

data Stats = Stats
  { statsTotalPositions :: Sum Int,
    statsTotalSum :: Sum Int,
    statsAbsoluteMax :: Max Int,
    statsAbsoluteMin :: Min Int,
    statsSellMax :: Maybe (Max Int),
    statsSellMin :: Maybe (Min Int),
    statsBuyMax :: Maybe (Max Int),
    statsBuyMin :: Maybe (Min Int),
    statsLongest :: MaxLen
  }
  deriving (Show, Eq)

strictMaybeCombine :: (Semigroup a) => Maybe a -> Maybe a -> Maybe a
strictMaybeCombine Nothing y = y
strictMaybeCombine x Nothing = x
strictMaybeCombine (Just !x) (Just !y) = Just $! (x <> y)

instance Semigroup Stats where
  Stats a1 b1 c1 d1 e1 f1 g1 h1 i1
    <> Stats a2 b2 c2 d2 e2 f2 g2 h2 i2 =
      Stats
        (a1 <> a2)
        (b1 <> b2)
        (c1 <> c2)
        (d1 <> d2)
        (strictMaybeCombine e1 e2)
        (strictMaybeCombine f1 f2)
        (strictMaybeCombine g1 g2)
        (strictMaybeCombine h1 h2)
        (i1 <> i2)

rowToStats :: Row -> Stats
rowToStats (Row pname ptype pcost) = case ptype of
  Sell -> Stats (Sum 1) (Sum pcost) (Max pcost) (Min pcost) (Just (Max pcost)) (Just (Min pcost)) Nothing Nothing (MaxLen pname)
  Buy -> Stats (Sum 1) (Sum (-pcost)) (Max pcost) (Min pcost) Nothing Nothing (Just (Max pcost)) (Just (Min pcost)) (MaxLen pname)

displayStats :: Stats -> String
displayStats stats =
  "Total positions        : "
    ++ show (getSum $ statsTotalPositions stats)
    ++ "\n"
    ++ "Total final balance    : "
    ++ show (getSum $ statsTotalSum stats)
    ++ "\n"
    ++ "Biggest absolute cost  : "
    ++ show (getMax $ statsAbsoluteMax stats)
    ++ "\n"
    ++ "Smallest absolute cost : "
    ++ show (getMin $ statsAbsoluteMin stats)
    ++ "\n"
    ++ "Max earning            : "
    ++ maybe "no value" (show . getMax) (statsSellMax stats)
    ++ "\n"
    ++ "Min earning            : "
    ++ maybe "no value" (show . getMin) (statsSellMin stats)
    ++ "\n"
    ++ "Max spending           : "
    ++ maybe "no value" (show . getMax) (statsBuyMax stats)
    ++ "\n"
    ++ "Min spending           : "
    ++ maybe "no value" (show . getMin) (statsBuyMin stats)
    ++ "\n"
    ++ "Longest product name   : "
    ++ unMaxLen (statsLongest stats)

reducer :: Stats -> String -> Stats
reducer acc s = maybe acc (\r -> acc <> rowToStats r) (parseRow s)

getFirst :: String -> Maybe Stats
getFirst s = parseRow s >>= \r -> Just (rowToStats r)

start :: [String] -> IO ()
start = go
  where
    go (x : xs) = case getFirst x of
      Nothing -> go xs
      Just s -> printProductStats s xs
    go [] = putStrLn "empty file"

printProductStats :: Stats -> [String] -> IO ()
printProductStats s xs = putStrLn $ displayStats $ foldl' reducer s xs

main :: IO ()
main = do
  args <- getArgs
  print args
  handler <- openFile (head args) ReadMode
  content <- hGetContents handler
  start $ lines content
