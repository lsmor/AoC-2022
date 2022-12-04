module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- lines <$> readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      print "not implemented"
    else do
      print "solution to problem 2 is:"
      print "not implemented"

