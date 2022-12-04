module Main where

import System.Environment (getArgs)
import Data.IntegerInterval
    ( relate, (<=..<=), IntegerInterval, Extended(Finite), isSubsetOf, isConnected, (==?), intersection, null )
import Data.IntervalRelation
    ( Relation(..) )
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as BS
import Prelude hiding (null)

parseIntervals :: Parser IntegerInterval
parseIntervals = (<=..<=) <$> (Finite <$> P.decimal) <*> (P.char '-' >> Finite <$> P.decimal)

parseIntervalPair :: Parser (IntegerInterval, IntegerInterval)
parseIntervalPair = (,) <$> parseIntervals <*> (P.char ',' >> parseIntervals)

parseInput :: Parser [(IntegerInterval, IntegerInterval)]
parseInput = parseIntervalPair `P.sepBy` P.endOfLine

wasted :: IntegerInterval -> IntegerInterval -> Bool
wasted i j = i `isSubsetOf` j || j `isSubsetOf` i

wastedWithOverlap :: IntegerInterval -> IntegerInterval -> Bool
wastedWithOverlap = (==?)

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- BS.readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      print $ sum . fmap (fromEnum . uncurry wasted) <$> P.parseOnly parseInput input
    else do
      print "solution to problem 2 is:"
      print $ sum . fmap (fromEnum . uncurry wastedWithOverlap) <$> P.parseOnly parseInput input

