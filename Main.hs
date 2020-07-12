module Main where

import qualified Parser

import Control.Monad.Trans
import System.Console.Haskeline

parse :: String -> IO ()
parse line = do
  let res = Parser.parseTop line
  case res of
    Left err -> print err
    Right ex -> print ex

-- input haskeline
main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> do
        liftIO (parse input)
        loop
