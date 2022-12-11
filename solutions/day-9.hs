module Main where

import System.Environment (getArgs)
import Data.Matrix (Matrix, prettyMatrix)
import Data.Matrix qualified as M
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as P
import Control.Applicative ((<|>))
import Data.ByteString qualified as BS
import Data.List (nub, elemIndex)
import Data.Foldable (foldl')
import Data.Char (digitToInt, intToDigit)

data Move = L | R | U | D deriving (Show, Eq)
type Coord = (Int, Int)
--   |           head  tail
data Rope = Rope Coord Coord deriving (Show, Eq)
type LongRope = [Rope]

ropeHead :: Rope -> Coord
ropeHead (Rope x0 _) = x0

ropeTail :: Rope -> Coord
ropeTail (Rope _ x1) = x1

pullTail :: Rope -> Rope
pullTail r@(Rope (x1, y1) (x2, y2))
  -- same line distance two
  | x1 == x2, y1 == y2     = r
  | x1 == x2, y1 == y2 + 2 = Rope (x1, y1) (x2, y2 + 1)
  | x1 == x2, y1 == y2 - 2 = Rope (x1, y1) (x2, y2 - 1)
  | x1 == x2 + 2, y1 == y2 = Rope (x1, y1) (x2 + 1, y2)
  | x1 == x2 - 2, y1 == y2 = Rope (x1, y1) (x2 - 1, y2)
  -- head right up distance two
  | x1 == x2 + 1, y1 == y2 + 2 = Rope (x1, y1) (x2 + 1, y2 + 1)
  | x1 == x2 + 2, y1 == y2 + 1 = Rope (x1, y1) (x2 + 1, y2 + 1)
  | x1 == x2 + 2, y1 == y2 + 2 = Rope (x1, y1) (x2 + 1, y2 + 1)
  -- head right down distance two
  | x1 == x2 + 1, y1 == y2 - 2 = Rope (x1, y1) (x2 + 1, y2 - 1)
  | x1 == x2 + 2, y1 == y2 - 1 = Rope (x1, y1) (x2 + 1, y2 - 1)
  | x1 == x2 + 2, y1 == y2 - 2 = Rope (x1, y1) (x2 + 1, y2 - 1)
  -- head left up distance two
  | x1 == x2 - 1, y1 == y2 + 2 = Rope (x1, y1) (x2 - 1, y2 + 1)
  | x1 == x2 - 2, y1 == y2 + 1 = Rope (x1, y1) (x2 - 1, y2 + 1)
  | x1 == x2 - 2, y1 == y2 + 2 = Rope (x1, y1) (x2 - 1, y2 + 1)
  -- head left down distance two
  | x1 == x2 - 1, y1 == y2 - 2 = Rope (x1, y1) (x2 - 1, y2 - 1)
  | x1 == x2 - 2, y1 == y2 - 1 = Rope (x1, y1) (x2 - 1, y2 - 1)
  | x1 == x2 - 2, y1 == y2 - 2 = Rope (x1, y1) (x2 - 1, y2 - 1)
  -- diagonal case distance one
  | otherwise = r

moveHead :: Move -> Rope -> Rope
moveHead m (Rope (n,i) t) = case m of
  L -> Rope (n - 1, i) t
  R -> Rope (n + 1, i) t
  U -> Rope (n, i + 1) t
  D -> Rope (n, i - 1) t

moveRope :: Rope -> [Move] -> [Coord]
moveRope rope []      = [ropeTail rope]
moveRope rope (m:ms)  =
  let new_rope = pullTail (moveHead m rope)
   in ropeTail rope:moveRope new_rope ms

propagateLongRope :: LongRope -> LongRope
propagateLongRope []     = []
propagateLongRope (r:rs) =
  let nr@(Rope _ t)    = pullTail r
      f []             = []
      f (Rope h t':xs) = Rope t t':xs
  in nr:propagateLongRope (f rs)

moveLongRope :: LongRope -> [Move] -> [Coord]
moveLongRope rope []      = [ropeTail (last rope)]
moveLongRope []   _       = []
moveLongRope rope@(r:rs) (m:ms)  =
  let new_rope = propagateLongRope (moveHead m r:rs)
   in ropeTail (last rope):moveLongRope new_rope ms

parseMove :: Parser [Move]
parseMove = do
  m <- U <$ P.char 'U' <|> L <$ P.char 'L' <|> R <$ P.char 'R' <|> D <$ P.char 'D'
  P.skipSpace
  times <- P.decimal
  pure $ replicate times m

parseInput :: Parser [Move]
parseInput = concat <$> parseMove `P.sepBy` P.endOfLine

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- BS.readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      print $ length . nub . moveRope (Rope (0,0) (0,0)) <$> P.parseOnly parseInput input -- 
    else do
      print "solution to problem 2 is:"
      let long_rope = replicate 9 $ Rope (0,0) (0,0)
      print $ length . nub . moveLongRope long_rope <$> P.parseOnly parseInput input

