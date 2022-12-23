module Main where

import System.Environment (getArgs)
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as M
import Algorithm.Search ( bfs )
import Data.Maybe (isJust, catMaybes)
import Data.Foldable (find)
import Data.List (minimumBy)
import Data.Function (on)

type Heightmap = Matrix Int
type Pos = (Int, Int)

neighbours :: Pos -> [Pos]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

toHeight :: Char -> Int
toHeight 'S' = 0
toHeight 'E' = toHeight 'z' + 1
toHeight  c  = fromEnum c - fromEnum 'a' + 1

findShortestPath :: Heightmap -> Maybe [(Int, Int)]
findShortestPath hmap = bfs candidates is_top initial_state
  where candidates :: Pos -> [Pos]
        candidates p = [(x, y) | (x, y) <- neighbours p, isJust (M.safeGet x y hmap) && (hmap ! (x,y) - 1 <= hmap ! p) ]

        is_top :: Pos -> Bool
        is_top p = hmap ! p == toHeight 'E'

        initial_state :: Pos
        initial_state = let Just (p, _) = find ((== 0) . snd) $ M.mapPos (,) hmap in p

findShortestPaths :: Heightmap -> [Maybe [(Int, Int)]]
findShortestPaths hmap = bfs candidates is_top <$> initial_states
  where candidates :: Pos -> [Pos]
        candidates p = [(x, y) | (x, y) <- neighbours p, isJust (M.safeGet x y hmap) && (hmap ! (x,y) - 1 <= hmap ! p) ]

        is_top :: Pos -> Bool
        is_top p = hmap ! p == toHeight 'E'

        initial_states :: [Pos]
        initial_states = fmap fst . filter ((<= 1) . snd) . M.toList $ M.mapPos (,) hmap



main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- M.fromLists . fmap (fmap toHeight) . lines <$> readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      let Just shortest = findShortestPath input
      print shortest
      print $ length shortest
    else do
      print "solution to problem 2 is:"
      print $ minimum .  fmap length . catMaybes $ findShortestPaths input

