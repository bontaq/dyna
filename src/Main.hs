{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.RawString.QQ
import Text.Trifecta
import Control.Monad


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

data Graph = Graph Path

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

main :: IO ()
main = do
  putStrLn . show $ parseString (many (try parsePath)) mempty exampleImports
  putStrLn "hello world"
