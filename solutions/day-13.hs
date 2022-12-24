module Main where

import System.Environment (getArgs)
import Debug.Trace (trace)
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as P
import Control.Applicative ((<|>), Applicative (liftA2))
import Data.ByteString qualified as B
import Data.List (sortOn, sortBy, elemIndex)
import Data.Function (on)

data Atom = AtomI Int | AtomL [Atom] deriving (Show, Eq)
data Validation = Ok | Fail | Continue deriving (Show, Eq, Ord)
data Pair = Pair {left :: Atom, right :: Atom} deriving (Show, Eq)

validate :: Atom -> Atom -> Validation
validate (AtomI n) (AtomI i)
 | n < i = Ok
 | n > i = Fail
 | otherwise = Continue
validate (AtomI n) (AtomL ats) = validate (AtomL [AtomI n]) (AtomL ats)
validate (AtomL ats) (AtomI n) = validate (AtomL ats)       (AtomL [AtomI n])
validate (AtomL []) (AtomL []) = Continue
validate (AtomL []) (AtomL (at : ats)) = Ok
validate (AtomL (at : ats2)) (AtomL [])          = Fail
validate (AtomL (at : ats2)) (AtomL (at' : ats)) =
  case validate at at' of
    Ok -> Ok
    Fail -> Fail
    Continue -> validate (AtomL ats2) (AtomL ats)

checkPacket :: Pair -> Validation
checkPacket (Pair at at') = at `validate` at'

parsePacket :: Parser Atom
parsePacket = parseInt <|> parseList
  where parseInt = AtomI <$> P.decimal
        parseList = AtomL <$> (P.char '[' *> (parsePacket `P.sepBy` P.char ',') <* P.char ']')

parsePair :: Parser Pair
parsePair = Pair <$> parsePacket <*> (P.endOfLine *> parsePacket)

parseInput :: Parser [Pair]
parseInput = parsePair `P.sepBy` (P.endOfLine *> P.endOfLine)

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- B.readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      let Right pairs = P.parseOnly parseInput input
      print $ sum . fmap fst . filter ((== Ok) . snd) $ zip [1..] (fmap checkPacket pairs)
    else do
      print "solution to problem 2 is:"
      let Right pairs = P.parseOnly parseInput input
          divider2 = AtomL [AtomL [AtomI 2]]
          divider6 = AtomL [AtomL [AtomI 6]]
          ordered_packets = sortBy (\a b -> compare (a `validate` b) Ok) $ divider2 : divider6 : concat [[x,y] | Pair x y <- pairs]
      print $ liftA2 (*) ((+1) <$> elemIndex divider2 ordered_packets) ((+1) <$> elemIndex divider6 ordered_packets)

