module Main where

import Data.Char ( ord, isLower )
import System.Environment (getArgs)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List.Split (chunksOf)

charToPriority :: Char -> Int
charToPriority c = if isLower c then ord c - 96 else ord c - 64 + 26

makeBag :: String -> (Set Char, Set Char)
makeBag s = let (c1, c2) = splitAt (length s `div` 2) s in (Set.fromList c1, Set.fromList c2)

bagToPrority :: (Set Char, Set Char) -> Int
bagToPrority (c1, c2) = sum $ Set.map charToPriority (c1 `Set.intersection` c2)

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- lines <$> readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      print $ sum $ fmap (bagToPrority . makeBag) input
    else do
      print "solution to problem 2 is:"
      print $ sum $ fmap (sum . Set.map charToPriority . foldr1 Set.intersection) $ chunksOf 3 $ fmap Set.fromList input
      
