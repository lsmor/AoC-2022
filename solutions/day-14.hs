{-# LANGUAGE NumericUnderscores #-}
module Main where

import System.Environment (getArgs)
import Data.Map (Map)
import Data.Map qualified as M
import Control.Monad.State.Strict (State, gets, put, modify', StateT (runStateT), get)
import Control.Monad (guard)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.List.Split qualified as Split
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.Foldable (foldl')
import Data.List ( foldl1', transpose )
import Data.Semigroup ( Max(Max), Min(Min) )
import Control.Concurrent (threadDelay)

data Tile = Air | Rock | Sand deriving (Show, Eq)
type Coord = (Int, Int)
data SandState = Drop Coord | Rest Coord
type CaveMap = Map Coord Tile
type Simulation a = StateT CaveMap IO a

prettyPrint :: CaveMap -> ByteString
prettyPrint cave = B.unlines $ foldMap (prettytile . snd) <$> transpose (Split.split onybound $ M.toAscList cave)
 where ((_, ybound), _) = head $ M.toDescList cave
       onybound = Split.keepDelimsR $ Split.whenElt ((== ybound ) . snd . fst)
       prettytile Air = ". "
       prettytile Rock = "# "
       prettytile Sand = "O "


dropSand :: SandState -> Simulation ()
dropSand (Rest c)     = modify' (M.insert c Sand) >> dropSand (Drop (500,0))
dropSand (Drop (x,y)) = do
  --liftIO $ threadDelay 100_000
  --liftIO $ putStr "\ESC[2J"

  cave <- get
  let down  = (x, y+1)
      left  = (x-1, y+1)
      right =  (x+1, y+1)
      m_down_tile = cave M.!? down
      m_left_tile = cave M.!? left
      m_right_tile = cave M.!? right

  --liftIO $ B.putStr $ prettyPrint cave

  modify' $ M.insert (x,y) Air
  case m_down_tile of
    Nothing  -> pure ()
    Just Air -> modify' (M.insert down Sand) >> dropSand (Drop down)
    _other   ->
       case m_left_tile of
          Nothing  -> pure ()
          Just Air -> modify' (M.insert left Sand) >> dropSand (Drop left)
          _other ->
            case m_right_tile of
              Nothing  -> pure ()
              Just Air -> modify' (M.insert right Sand) >> dropSand (Drop right)
              _other   -> 
                if (x,y) == (500,0)
                  then liftIO (putStrLn "Good result") >> pure ()
                  else dropSand $ Rest (x,y)


coords2CaveMap :: [Coord] -> CaveMap
coords2CaveMap cs = M.fromList $ concatMap build_map $ zip cs (tail cs)
  where build_map ((x,y), (x', y'))
          | x == x' = [ ((x,z), Rock) | z <- [min y y'.. max y y'] ]
          | y == y' = [ ((z,y), Rock) | z <- [min x x'.. max x x'] ]
          | otherwise = error "the imposible happened"

makeFullCave :: CaveMap -> CaveMap
makeFullCave cave = cave `M.union` air_map `M.union` floor_map
  where
    (Min xmin, Max xmax, Max ymax) = foldMap (\(x, y) -> (Min x, Max x, Max y)) $ M.keys cave
    air_map = M.fromList [((x,y), Air) | x <- [xmin - 50000 .. xmax + 50000], y <- [0 .. ymax +1]]
    floor_map = M.fromList [((x, ymax + 2), Rock) | x <- [xmin - 50000 .. xmax + 50000]]

parserScan :: Parser [Coord]
parserScan = parseCoord `P.sepBy` parseSep
  where parseCoord = (,) <$> P.decimal <*> (P.char ',' *> P.decimal)
        parseSep = P.string " -> "

parseInput :: Parser [[Coord]]
parseInput = parserScan `P.sepBy` P.endOfLine

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- B.readFile filepath
  let Right coords = P.parseOnly parseInput input
      cave = makeFullCave $ foldl' M.union M.empty (fmap coords2CaveMap coords)
      start_simulation = dropSand $ Drop (500, 0)
  if read @Int part == 1
    then do
      (_,cave_final) <- runStateT start_simulation cave
      print "solution to problem 1 is:"
      print $ length . filter ((== Sand) . snd) $ M.toList cave_final
    else do
      (_,cave_final) <- runStateT start_simulation cave
      print "solution to problem 2 is:"
      print $ length . filter ((== Sand) . snd) $ M.toList cave_final

