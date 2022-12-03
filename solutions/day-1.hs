module Main where
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import Data.Foldable ( Foldable(foldl') )
import Data.List.Split ( splitWhen )
import Data.List (sort, sortOn)
import Data.Ord (Down(Down))


main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- lines <$> readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      print $  maximum $ sum . fmap (read @Int) <$> splitWhen (== "") input
    else do
      print "solution to problem 2 is:"
      print $ sum $ take 3 $ sortOn Down $ sum . fmap (read @Int) <$> splitWhen (== "") input