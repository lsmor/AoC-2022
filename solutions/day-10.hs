{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import System.Environment (getArgs)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as P
import Control.Applicative ((<|>))
import Data.Monoid (Sum(..))
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Matrix (Matrix)
import Data.Matrix qualified as M
import Data.List.Split (chunksOf)

data Instruction a = Add a | Noop deriving (Show, Eq, Functor)
type Program = [Instruction Int]
data Pixel = Lit | Dark deriving (Show, Eq)
--                       Regis and Current Pixel
data Register = Register Int Int deriving (Show, Eq)

instance Semigroup a => Semigroup (Instruction a) where
  (<>) :: Semigroup a => Instruction a -> Instruction a -> Instruction a
  (Add a) <> (Add a') = Add (a <> a')
  (Add n) <> Noop = Add n
  Noop <> (Add n) = Add n
  Noop <> Noop = Noop

instance Semigroup a => Monoid (Instruction a) where
  mempty :: Semigroup a => Instruction a
  mempty = Noop

parseInstruction :: Parser (Instruction Int)
parseInstruction = parseAdd <|> (Noop <$ P.string "noop")
  where parseAdd = fmap Add (P.string "addx" *> P.skipSpace *> P.signed P.decimal)

parseInput :: Parser Program
parseInput = parseInstruction `P.sepBy` P.endOfLine

cycles :: Program -> Program
cycles [] = []
cycles ((Add n) : ins') = Noop:Add n:cycles ins'
cycles (Noop : ins') = Noop:cycles ins'

instructionValue :: Monoid a => Instruction a -> a
instructionValue (Add a) = a
instructionValue Noop = mempty

calculateStrength :: Int -> Program -> Int
calculateStrength n p =  n * (1 + x)
  where cs = take (n-1) $ cycles p
        x  = getSum . instructionValue $ foldMap (fmap Sum) cs

blankLine :: Int -> Matrix Pixel
blankLine n = M.fromList 1 n (replicate n Dark)

line :: Program -> Matrix Pixel -> Register -> (Register , Matrix Pixel)
line [] m r = (r, m)
line (i:is) m (Register sprite_register crt) =
  let pix = if crt `elem` [sprite_register - 1 , sprite_register, sprite_register + 1] then Lit else Dark
      new_matrix = M.setElem pix (1,crt) m
      sprite_move = getSum . instructionValue $ fmap Sum i
      new_register = Register (sprite_register + sprite_move) (crt + 1)
   in line is new_matrix new_register 

-- >>> let p_bkup = [Add 15,Add (-11),Add 6,Add (-3),Add 5,Add (-1),Add (-8),Add 13,Add 4,Noop,Add (-1),Add 5,Add (-1),Add 5,Add (-1),Add 5,Add (-1),Add 5,Add (-1),Add (-35),Add 1,Add 24,Add (-19),Add 1,Add 16,Add (-11),Noop,Noop,Add 21,Add (-15),Noop,Noop,Add (-3),Add 9,Add 1,Add (-3),Add 8,Add 1,Add 5,Noop]
-- >>> let p = take 9 (cycles p_bkup)
-- >>> let (reg, m) = line p (blankLine 9) (Register 2 1) 
-- >>> (reg, fmap pprint m)
-- (Register 9 10,
-- ┌                                     ┐
-- │ '#' '#' '.' '.' '#' '#' '.' '.' '#' │
-- └                                     ┘)


plot :: [Program] -> Register -> Matrix Pixel
plot [] _ = blankLine 40
plot (p:ps) (Register sprite_register crt) = 
  let (last_register, l) = line p (blankLine 40) (Register sprite_register 1) 
   in l M.<-> plot ps last_register

pprint :: Matrix Pixel -> String
pprint m = unlines $ M.toLists $ fmap f m 
 where f Lit = '#'
       f Dark = '.'


main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- BS.readFile filepath
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      print $ do
        let Right p = P.parseOnly parseInput input
        sum $ ($ p) <$> fmap calculateStrength [20,60 .. 220]
    else do
      print "solution to problem 2 is:"
      let Right p = P.parseOnly parseInput input
      putStrLn $ pprint $ plot (chunksOf 40 $ cycles p) (Register 2 1)

