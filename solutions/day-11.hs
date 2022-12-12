{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE NumericUnderscores #-}
module Main where

import Data.Sequence (Seq, Seq((:<|), (:|>)))
import Data.Sequence qualified as S
import Data.Map ( Map )
import Data.Map qualified as M
import Control.Monad.State.Strict ( State, get, put, gets, modify, runState, evalState, execState)
import System.Environment (getArgs)
import Data.Foldable (traverse_, foldl')
import Data.Semigroup (Sum (..))
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as P
import Control.Applicative (Applicative(..), Alternative ((<|>)))
import Data.ByteString qualified as BS
import Data.List (sortOn, sort)

type WorryLevel = Int
type MonkeyIndex = Int
data Test = Test {cond :: Int, positive :: MonkeyIndex, negative :: MonkeyIndex}
data MonkeySpec = MonkeySpec
  { items :: Seq WorryLevel
  , op :: WorryLevel -> WorryLevel
  , test :: Test
  }
type Jungle = Map MonkeyIndex MonkeySpec
type MonkeyBusiness a = State Jungle a

insertItem :: WorryLevel -> MonkeySpec -> MonkeySpec
insertItem w ms@MonkeySpec {..} = ms{items = items :|> w}

turn :: MonkeyIndex -> MonkeyBusiness (Map MonkeyIndex (Sum Int))
turn n = do
  MonkeySpec {..} <- gets (M.! n)
  case items of
    S.Empty  -> pure $ M.singleton n (Sum 0)
    w :<| ws -> do
      let new_worry = op w `div` 3
      modify $ M.adjust (\ms -> ms{items = ws}) n
      if new_worry `rem` cond test == 0
        then sendItem (positive test) new_worry
        else sendItem (negative test) new_worry
      M.unionWith (<>) (M.singleton n (Sum 1)) <$> turn  n

sendItem :: MonkeyIndex -> WorryLevel -> MonkeyBusiness ()
sendItem i w = modify $ M.adjust (insertItem w) i

turnModular :: MonkeyIndex -> MonkeyBusiness (Map MonkeyIndex (Sum Int))
turnModular n = do
  MonkeySpec {..} <- gets (M.! n)
  case items of
    S.Empty  -> pure $ M.singleton n (Sum 0)
    w :<| ws -> do
      let new_worry = op w
      modify $ M.adjust (\ms -> ms{items = ws}) n
      if new_worry `rem` cond test == 0
        then sendItemModular (positive test) new_worry
        else sendItemModular (negative test) new_worry
      M.unionWith (<>) (M.singleton n (Sum 1)) <$> turnModular n

sendItemModular :: MonkeyIndex -> WorryLevel -> MonkeyBusiness ()
sendItemModular i w = do
  mcd <- gets $ product . fmap (cond . test . snd) . M.toList
  let new_worry = w `rem` mcd
  modify $ M.adjust (insertItem new_worry) i


playRound :: (MonkeyIndex -> MonkeyBusiness (Map MonkeyIndex (Sum Int)))-> MonkeyBusiness (Map MonkeyIndex (Sum Int))
playRound turn_play = do
  s <- gets M.size
  counts <- traverse turn_play [0..s-1]
  pure $ foldl' (M.unionWith (<>)) M.empty counts


parseMonkeyIndex :: Parser MonkeyIndex
parseMonkeyIndex = P.string "Monkey " *> P.decimal <* P.char ':' <* P.endOfLine

parseItems :: Parser (Seq WorryLevel)
parseItems = P.string "  Starting items: " *> (S.fromList <$> P.decimal `P.sepBy` P.string ", ") <* P.endOfLine

parseOperation :: Parser (WorryLevel -> WorryLevel)
parseOperation = do
  P.string "  Operation: new = old "
  let sumP  = (+) <$> (P.char '+' *> P.skipSpace *> P.decimal)
      prodP = (*) <$> (P.char '*' *> P.skipSpace *> P.decimal)
      expP  = (^2) <$ (P.char '*' <* P.skipSpace <* P.string "old")
  (sumP <|> prodP <|> expP) <* P.endOfLine

parseTestCond :: Parser Int
parseTestCond = P.string "  Test: divisible by " *> P.decimal  <* P.endOfLine

parseTestPositive :: Parser MonkeyIndex
parseTestPositive = P.string "    If true: throw to monkey " *> P.decimal  <* P.endOfLine

parseTestNegative :: Parser MonkeyIndex
parseTestNegative = P.string "    If false: throw to monkey " *> P.decimal  <* P.endOfLine

parseTest :: Parser Test
parseTest = Test <$> parseTestCond <*> parseTestPositive <*> parseTestNegative

parseSpec :: Parser MonkeySpec
parseSpec = MonkeySpec <$> parseItems <*> parseOperation <*> parseTest

parseInput :: Parser Jungle
parseInput = foldr M.union M.empty <$> (liftA2 M.singleton parseMonkeyIndex parseSpec `P.sepBy` P.endOfLine)

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- BS.readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      let Right j = P.parseOnly parseInput input
          play = traverse (const $ playRound turn) [1..20]
          active_monkeys = foldl' (M.unionWith (<>)) M.empty $ evalState play j
      print $ sort . fmap (getSum . snd) $ M.toList active_monkeys
    else do
      print "solution to problem 2 is:"
      let Right j = P.parseOnly parseInput input
          play = traverse (const $ playRound turnModular) [1..10_000]
          active_monkeys = foldl' (M.unionWith (<>)) M.empty $ evalState play j
      print $ sort . fmap (getSum . snd) $ M.toList active_monkeys
