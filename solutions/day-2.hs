module Main where

import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative (Alternative((<|>)))
import System.Environment (getArgs)
import qualified Data.ByteString as BS

data GameChoice = Rock | Paper | Scissors  deriving (Show, Eq)
data GameOutput = Win | Loose | Draw deriving (Show, Eq)
data GameStrategy a = GameStrategy {opponent :: GameChoice, player :: a} deriving (Eq, Show)

beats :: GameChoice -> GameChoice -> GameOutput
beats Rock Scissors = Win
beats Scissors Paper = Win
beats Paper Rock = Win
beats c1 c2 = if c1 == c2 then Draw else Loose

needs :: GameChoice -> GameOutput -> GameChoice
needs Rock Win     = Paper
needs Paper Win    = Scissors
needs Scissors Win = Rock
needs Rock Loose     = Scissors
needs Paper Loose    = Rock
needs Scissors Loose = Paper
needs c Draw = c

choiceToPoints :: GameChoice -> Int
choiceToPoints Rock     = 1
choiceToPoints Paper    = 2
choiceToPoints Scissors = 3

outputToPoints :: GameOutput -> Int
outputToPoints Win   = 6
outputToPoints Draw  = 3
outputToPoints Loose = 0

strategyToPoint :: GameStrategy GameChoice -> Int
strategyToPoint s = choiceToPoints s.player + outputToPoints (s.player `beats` s.opponent)

strategyResolve :: GameStrategy GameOutput -> GameStrategy GameChoice
strategyResolve s = s{player = s.opponent `needs` s.player}

parseGameChoice :: P.Parser GameChoice
parseGameChoice = Rock     <$ (P.char 'A' <|> P.char 'X')
              <|> Paper    <$ (P.char 'B' <|> P.char 'Y')
              <|> Scissors <$ (P.char 'C' <|> P.char 'Z')

parseGameChoice' :: P.Parser GameChoice
parseGameChoice' = Rock     <$ P.char 'A'
               <|> Paper    <$ P.char 'B'
               <|> Scissors <$ P.char 'C'


parseGameOuput :: P.Parser GameOutput
parseGameOuput = Loose <$ P.char 'X'
              <|> Draw <$ P.char 'Y'
              <|> Win  <$ P.char 'Z'


parseStrategy :: P.Parser (GameStrategy GameChoice)
parseStrategy = GameStrategy <$> (parseGameChoice <* P.space) <*> parseGameChoice

parseInput :: P.Parser [GameStrategy GameChoice]
parseInput = parseStrategy `P.sepBy` P.endOfLine


parseStrategy' :: P.Parser (GameStrategy GameOutput)
parseStrategy' = GameStrategy <$> (parseGameChoice <* P.space) <*> parseGameOuput

parseInput' :: P.Parser [GameStrategy GameOutput]
parseInput' = parseStrategy' `P.sepBy` P.endOfLine



main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- BS.readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      print $ P.parseOnly (sum . fmap strategyToPoint <$> parseInput) input
    else do
      print "solution to problem 2 is:" 
      print $ P.parseOnly (sum . fmap (strategyToPoint . strategyResolve) <$> parseInput') input


