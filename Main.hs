module Main where

import Parser
import Codegen
import EmitLLVM

import Control.Monad.Trans
import System.Console.Haskeline
import System.IO
import System.Environment

import qualified LLVM.AST as AST

process :: AST.Module -> String -> IO (Maybe AST.Module)
process mod src = do
    let res = parseTop src
    case res of
        Left err -> print err >> return Nothing
        Right ex -> do
            ast <- codegen mod ex
            return (Just ast)

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process (emptyModule "new")

-- input haskeline
repl :: IO ()
repl = runInputT defaultSettings (loop (emptyModule "new"))
  where
  loop mod = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> do
        ret <- liftIO (process mod input)
        case ret of
            Just x -> loop x
            Nothing -> loop mod

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        [fname] -> processFile fname >> return ()
