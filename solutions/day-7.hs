{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (Foldable (..))
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Monoid (Sum (..))
import System.Environment (getArgs)
import Data.Function (on)
import Data.List (find)

type Name = ByteString
type Size = Int
data FileSystem a = File Name a | Dir Name [FileSystem a] deriving (Show, Eq, Foldable, Functor)
data Command = ChangeDir ByteString | ListDir [FileSystem Size] deriving (Show, Eq)

parseCommand :: Parser Command
parseCommand = P.string "$ " *> (changedirParser <|> lsParser)
 where
  changedirParser = ChangeDir <$> (P.string "cd " *> P.takeTill (== '\n'))
  lsParser = P.string "ls\n" *> (ListDir <$> parseListOutput `P.sepBy` P.endOfLine)

parseListOutput :: Parser (FileSystem Size)
parseListOutput = parseFile <|> parseDir
 where
  parseFile = do
    size <- P.decimal
    name <- P.space *> P.takeTill (== '\n')
    pure $ File name size
  parseDir = do
    P.string "dir "
    name <- P.takeTill (== '\n')
    pure $ Dir name []

parseInput :: Parser [Command]
parseInput = parseCommand `P.sepBy` P.endOfLine

getName :: FileSystem a -> Name
getName (File bs _) = bs
getName (Dir bs _) = bs

makeFullPath :: ByteString -> FileSystem a -> FileSystem a
makeFullPath bs (File bs' a)  = File (bs <> "/" <> bs') a
makeFullPath bs (Dir bs' fss) = Dir (bs <> "/" <> bs') fss

mergeDirs :: [FileSystem a] -> [FileSystem a] -> [FileSystem a]
mergeDirs [] [] = []
mergeDirs [] xs = xs
mergeDirs (e : fs) xs =
  case find ((lookup_name ==) . getName) xs of
    Nothing -> e:mergeDirs fs xs
    Just _  -> mergeDirs fs xs
 where lookup_name = getName e

setDirectories :: [FileSystem a] -> Name -> FileSystem a -> FileSystem a
setDirectories _    _           (File n s)       = File n s
setDirectories dirs lookup_name (Dir n sub_tree)
 | n == lookup_name = Dir n (mergeDirs dirs sub_tree)
 | otherwise =  Dir n (fmap (setDirectories dirs lookup_name) sub_tree)

buildFileSystem :: [Command] -> FileSystem Size
buildFileSystem (ChangeDir "/" : cs) = go ["/"] (Dir "/" []) cs
 where
  go visited_dirs fileSystem [] = fileSystem
  go visited_dirs fileSystem (ListDir r : commands) =
    let current_path = BS.intercalate "/" $ reverse visited_dirs
        full_folder = makeFullPath current_path <$> r
        new_fs      = setDirectories full_folder current_path fileSystem
     in go visited_dirs new_fs commands
  go visited_dirs fileSystem (ChangeDir ".." : commands) = go (tail visited_dirs) fileSystem commands
  go visited_dirs fileSystem (ChangeDir name : commands) =
      let current_path = BS.intercalate "/" $ reverse visited_dirs
          dir_path     = current_path <> "/" <> name 
          new_fs       = setDirectories [Dir dir_path []] current_path fileSystem
      in go (name : visited_dirs) new_fs commands

calculateDirSize :: FileSystem Size -> [(Name, Size)]
calculateDirSize (File _ _) = []
calculateDirSize d@(Dir n dirs) =
  let size = getSum $ foldMap Sum d
   in (n, size) : concatMap calculateDirSize dirs

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- BS.readFile filepath
  let commands = P.parseOnly parseInput input
      fs = buildFileSystem <$> commands
  if read @Int part == 1
    then do
      print "solution to problem 1 is:"
      print $ sum . filter (<= 100000) . fmap snd . calculateDirSize <$> fs
      -- print $ commands
      -- print $ fs
    else do
      print "solution to problem 2 is:"
      print "not implemented"
