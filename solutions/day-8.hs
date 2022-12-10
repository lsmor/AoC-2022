module Main where

import System.Environment (getArgs)
import Data.Char (digitToInt)
import Data.Matrix (Matrix)
import Data.Matrix qualified as M
import Data.Monoid (Sum(Sum))
import Data.Semigroup (Sum(getSum))

isVisible :: Matrix Int -> Matrix Bool
isVisible m = M.mapPos f m
 where f :: ((Int, Int) -> Int -> Bool) 
       f (r, c) current_elem = 
        let nrows = M.nrows m
            ncols = M.ncols m
            check_left   = all (< current_elem) $ M.submatrix r     r     1     (c-1) m
            check_right  = all (< current_elem) $ M.submatrix r     r     (c+1) ncols m
            check_top    = all (< current_elem) $ M.submatrix 1     (r-1) c     c     m
            check_bottom = all (< current_elem) $ M.submatrix (r+1) nrows c     c     m
         in r `elem` [1, nrows] || c `elem` [1, ncols] || check_left || check_right || check_top || check_bottom

sceneScore :: Matrix Int -> Matrix Int
sceneScore m = M.mapPos f m
 where f :: (Int, Int) -> Int -> Int
       f (r, c) current_elem = 
        let sum_visibles n c (count, continue) = 
              let new_continue = (c < n) && continue 
              in  (count + fromEnum continue, new_continue) 
            left cond  = if cond then foldl (flip $ sum_visibles current_elem) (0, True) else const (0, True)
            right cond = if cond then foldr (sum_visibles current_elem) (0, True) else const (0, True)
            nrows = M.nrows m
            ncols = M.ncols m
            slide_left   = fst . right (c > 1)     $ M.submatrix r     r     1     (c-1) m
            slide_right  = fst . left  (c < ncols) $ M.submatrix r     r     (c+1) ncols m
            slide_top    = fst . right (r > 1)     $ M.submatrix 1     (r-1) c     c     m
            slide_bottom = fst . left  (r < nrows) $ M.submatrix (r+1) nrows c     c     m
         in slide_left * slide_right * slide_top * slide_bottom

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- M.fromLists . fmap (fmap digitToInt) . lines <$> readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      print $ getSum $ foldMap (Sum . fromEnum) $ isVisible input
    else do
      print "solution to problem 2 is:"
      print $ maximum . sceneScore $ input

