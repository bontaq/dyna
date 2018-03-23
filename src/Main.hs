{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.IO.Class
-- import           Data.Maybe
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

type Path = String

parsePath :: Parser Path
parsePath = do
  _ <- skipMany (char '\n')
  _ <- string "import"
  _ <- skipMany (noneOf "'")
  _ <- skipSome (char '\'')
  p <- many (noneOf "'")
  _ <- skipSome (string "';\n")
  return $ p

findPaths :: Path -> IO (Either SomeException (Maybe [Path]))
findPaths p =
  try $ parseFromFile (many (T.try parsePath)) p

onPath :: (String -> String) -> Path -> Path
onPath fn p = fn p

-- walkPaths :: Path -> Path -> IO (Path, [Either SomeException (Maybe [Path])])
-- walkPaths base p = do
--   let path :: Path
--       path = concat [base, p]
--       cleanPath :: [Char] -> [Char]
--       cleanPath = filter (/= '.')
--       withTS = (flip (++) ".ts")
--   ps <- findPaths path
--   case ps of
--     Left e   -> return $ (path, [Left e])
--     Right (Just ps') ->
--       let cleanBase = '.' : cleanPath base
--           cleanPaths = fmap (onPath cleanPath) ps'
--           withTSPaths = fmap (onPath withTS) cleanPaths
--           newPaths = (fmap . fmap) (walkPaths cleanBase) [withTSPaths]
--           joined = join newPaths
--           sequenced = sequence joined
--       in
--         sequenced >>= (\seq -> return (path, seq))

data Tree a =
  Tree Path (Tree a)
  | Leaf [a]
  | TNothing
  deriving (Show)

handlePaths :: Either a1 (Maybe [a2]) -> Tree a2
handlePaths (Left _)           = TNothing
handlePaths (Right maybePaths) =
  case maybePaths of
    Just (paths) -> Leaf paths
    Nothing      -> TNothing

walkPaths :: Path -> IO (Tree Path)
walkPaths p = do
  let a = findPaths p
      b = a >>= (\n -> return $ handlePaths n)
  c <- b
  return $ Tree p c

walkPaths' p = do
  paths <- walkPaths p
  return ()

main :: IO ()
main = do
  putStrLn "sup"
  n <- walkPaths ("./data/index.ts")
  putStrLn . show $ n
