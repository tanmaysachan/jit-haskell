module Main where

import Parser
import Codegen
import EmitLLVM

import Control.Monad.Trans
import System.Console.Haskeline
import System.IO
import System.Environment

import qualified LLVM.AST as AST

process :: String -> IO ()
process src = do
    let res = parseTop src
    case res of
        Left err -> print err
        Right ex -> do
            mapM_ print ex

-- input haskeline
repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> (liftIO $ process input) >> loop

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
