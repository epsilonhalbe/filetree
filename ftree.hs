{-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}

module FTree where

import System.FilePath()
import System.IO
import System.Directory
import Control.Applicative ((<$>))
import Data.Either (rights)

--import Text.Parsec
import Text.ParserCombinators.Parsec

type ID = String

data File = File {_id :: ID, _path :: FilePath, _parents :: [ID]}
          deriving (Show)
data FTree = Null
           | Node { _file :: File
                  , _children :: [FTree]} deriving (Show)

main :: IO ()
main = return ()

createFT :: File -> IO FTree
createFT f = ifNExists f Null
               (withFile (_path f) ReadMode $ \handle ->
                  do let thisParse = fparse (_id f :_parents f)
                     children <-rights . map ( thisParse . trim) . lines <$> hGetContents handle
                     c <- mapM createFT children
                     return $ Node f c)

ifNExists :: File -> a -> IO a -> IO a
ifNExists f a b = do existsFile <- doesFileExist (_path f)
                     if not existsFile then return a else b

createF :: FTree -> IO String
createF Null = return ""
createF (Node f children) = ifNExists f (_id f)
                              (do childStrings <- mapM createF children
                                  withFile (_path f) ReadMode $ \handle ->
                                    do fc <- lines <$> hGetContents handle
                                       return $ merge fc childStrings)

root = File "root" "filenames" []
fft = createFT root


merge :: [String] ->  [String] -> String
merge x [] = unlines x
merge [] _ = []
merge (x:xx) ys@(y:yy) = unlines $ case fparse [] x
                                     of Right _ -> [y,merge xx yy]
                                        Left  _ -> [x,merge xx ys]

ltrim :: String -> String
ltrim [] = []
ltrim ccc@(c:cc) = if c `elem` "  \t" then ltrim cc
                                  else ccc
rtrim :: String -> String
rtrim = reverse . ltrim . reverse

trim :: String -> String
trim = rtrim . ltrim

fparse :: [ID] -> String -> Either ParseError File
fparse parents str = runParser (parseFile parents) () ("error: "++str) str

parseFile :: [ID] -> Parser File -- !{name}(/path/to/file)
parseFile parents = do char '!'
                       char '{'
                       name <- many1 (noneOf "{}")
                       char '}'
                       char '('
                       path <- many1 (noneOf "()")
                       char ')'
                       return $ File (trim name) (trim path) parents
