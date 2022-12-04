module Main where

import System.Environment (getArgs)
import Data.IntegerInterval
    ( relate, (<=..<=), IntegerInterval, Extended(Finite) )
import Data.IntervalRelation
    ( Relation(..) )
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as BS

parseIntervals :: Parser IntegerInterval
parseIntervals = (<=..<=) <$> (Finite <$> P.decimal) <*> (P.char '-' >> Finite <$> P.decimal)

parseIntervalPair :: Parser (IntegerInterval, IntegerInterval)
parseIntervalPair = (,) <$> parseIntervals <*> (P.char ',' >> parseIntervals)

parseInput :: Parser [(IntegerInterval, IntegerInterval)]
parseInput = parseIntervalPair `P.sepBy` P.endOfLine

wasted :: IntegerInterval -> IntegerInterval -> Bool
wasted i j =
  case i `relate` j of
    Starts -> True
    During -> True
    Finishes -> True
    Equal -> True
    StartedBy -> True
    Contains -> True
    FinishedBy -> True
    _ -> False

wastedWithOverlap :: IntegerInterval -> IntegerInterval -> Bool
wastedWithOverlap i j =
  case i `relate` j of
    Before -> False
    JustBefore -> False
    After -> False
    JustAfter -> False
    _ -> True

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

