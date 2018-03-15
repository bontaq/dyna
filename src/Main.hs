{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Text.RawString.QQ
import           Text.Trifecta


exampleImport = [r|
import * as thing from './Component';
|]

exampleImports = [r|
import * as thing from './Component';
import thing from '../Another';

export class Warbglglg {
}

mapStateToProps = (state: Blah) => {
  text: {
    shoutyHeader: "u <3 bike?"
  }
}
|]

--type Path = String;
data Path = Path String
            deriving (Eq, Show)

data Node = Node Path

parsePath :: Parser Path
parsePath = do
  _ <- skipMany (char '\n')
  _ <- string "import"
  _ <- skipMany (noneOf "'")
  _ <- skipSome (char '\'')
  p <- many (noneOf "'")
  _ <- skipSome (string "';\n")
  return $ Path p

findPaths :: Path -> IO (Maybe [Path])
findPaths (Path p) = parseFromFile (many (try parsePath)) p


walkPath :: Path -> [Path] -> IO [Maybe [Path]]
walkPath (Path a) (paths) = sequence $ map findPaths paths

walkPaths p = do
  ps <- findPaths p
  let n = (fmap . fmap) findPaths ps
      b = (fmap . fmap . fmap . fmap . fmap) walkPaths n
  return ps


main :: IO ()
main = do
  m <- findPaths $ Path "./data/index.ts"
--   n <- walkPath (Path "") m
--   putStrLn . show $ n
  putStrLn "bebe"
