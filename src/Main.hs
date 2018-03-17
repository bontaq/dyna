{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.IO.Class
import           Text.RawString.QQ
import           Text.Trifecta          hiding (try)
import qualified Text.Trifecta          as T



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

--findPaths :: Path -> IO (Maybe [Path])
--findPaths :: (MonadIO m, Parsing m) => Path -> m (Maybe [Path])
findPaths :: Path -> IO (Either SomeException (Maybe [Path]))
findPaths (Path p) =
  try $ parseFromFile (many (T.try parsePath)) p

-- findPaths' p = do
--   result <- findPaths' p
--   return ()



-- walkPath :: Path -> [Path] -> IO [Maybe [Path]]
-- walkPath (Path a) (paths) = sequence $ map findPaths paths

-- ok, seems like we need a real parser for the files?
-- secondly, we need errors

-- ./data
-- index
-- finds: ./pages/Landing

onPath :: (String -> String) -> Path -> Path
onPath fn (Path p) = Path $ fn p

-- walkPaths :: Path -> Path -> IO [Maybe Path]
walkPaths :: Path -> Path -> IO [Either SomeException (Maybe [Path])]
walkPaths (Path base) (Path p) = do
  let path = Path $ concat [base, p]
      cleanPath :: [Char] -> [Char]
      cleanPath = filter (/= '.')
      withTS = (flip (++) ".ts")
  ps <- findPaths path
  case ps of
    Left e   -> return $ [Left e]
    Right (Just ps') ->
      -- FUCK YEAH HASKELL
      let cleanBase = Path $ '.' : cleanPath base
          cleanPaths = fmap (onPath cleanPath) ps'
          withTSPaths = fmap (onPath withTS) cleanPaths
      in
        fmap concat $ sequence . join $ (fmap . fmap) (walkPaths cleanBase) [withTSPaths]

main :: IO ()
main = do
  --m <- findPaths $ Path "./data/index.ts"
  n <- walkPaths (Path "./data") (Path "/index.ts")
  putStrLn . show $ n
  --putStrLn .show $ m
