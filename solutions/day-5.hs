{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Environment (getArgs)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Control.Monad.State.Strict ( State, gets, MonadState (get, state), modify, execState )
import Data.Foldable (traverse_, Foldable (foldl'))
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.Attoparsec.ByteString (Parser)
import Data.List.Split (chunksOf, splitWhen)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Char (isDigit)

type Crate = Char
type CrateStack = [Crate]
type Supplies = Map Int CrateStack
data Move = Move {from :: Int , to :: Int , many :: Int} deriving (Show, Eq)

type Rearrangement a = State Supplies a

pull :: Int -> CrateStack -> [Crate]
pull = take

push :: [Crate] -> CrateStack -> CrateStack
push [] s = s
push (x:xs) stack = push xs (x:stack)

rearrange :: Move -> Rearrangement ()
rearrange Move {..} = do
  crate <- gets (! from)
  let pulled = pull many crate
  modify $ M.adjust (drop many) from
  modify $ M.adjust (push pulled) to

rearrange' :: Move -> Rearrangement ()
rearrange' Move {..} = do
  crate <- gets (! from)
  let pulled = pull many crate
  modify $ M.adjust (drop many) from
  modify $ M.adjust (pulled ++) to

run :: [Move] -> Supplies -> Supplies
run moves sups = execState (traverse_ rearrange moves) sups

run' :: [Move] -> Supplies -> Supplies
run' moves sups = execState (traverse_ rearrange' moves) sups

parseCrate :: String -> Maybe Crate
parseCrate ('[':c:']':_) = Just c
parseCrate _ = Nothing

ingestSupplies :: Supplies -> [(Int, Crate)]  -> Supplies
ingestSupplies = foldl' f
 where f :: Supplies -> (Int, Crate) -> Supplies
       f m (k, c) = M.insertWith (++) k [c] m

parseMove :: String -> Move
parseMove s = Move (read @Int f) (read @Int t) (read @Int moves)
  where (moves, s') = span isDigit (drop 5 s)
        (f, s'') = span isDigit (drop 6 s')
        (t, _) = span isDigit (drop 4 s'')

solution :: Supplies -> String
solution = fmap (head . snd) . M.toAscList

main :: IO ()
main = do
  [part, filepath] <- getArgs
  (supplies_input, moves_input) <- break isDigit <$> readFile filepath
  let initial_supplies = foldr (flip ingestSupplies) M.empty .  fmap (mapMaybe sequence . zip [1..] . fmap parseCrate . chunksOf 4) . lines $ supplies_input
      moves = fmap parseMove . drop 2 . lines $ moves_input
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      let final_suplies = run moves initial_supplies
      print final_suplies
      print (solution final_suplies)
    else do
      print "solution to problem 2 is:"
      let final_suplies = run' moves initial_supplies
      print final_suplies
      print (solution final_suplies)

