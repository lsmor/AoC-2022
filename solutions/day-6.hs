module Main where

import System.Environment (getArgs)
import Data.List (nub)
import Data.List.Split (divvy)

type Datastream = String

isMarker :: Int -> Datastream -> Bool
isMarker n ds = length (nub ds) == n

markerPosition :: Int -> Datastream -> Int
markerPosition n ds = go chopped
  where chopped = divvy n 1 ds
        go [] = 0
        go (x:xs) = if isMarker n x then n else 1 + go xs


main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- lines <$> readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      print $ fmap (markerPosition 4) input
    else do
      print "solution to problem 2 is:"
      print $ fmap (markerPosition 14) input

