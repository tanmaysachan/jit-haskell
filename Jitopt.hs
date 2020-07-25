module Jitopt where

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Except

import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod
import qualified LLVM.AST as AST

import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

import qualified LLVM.ExecutionEngine as EE
import Utils

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
    where
        optlevel = Just 0
        model = Nothing
        ptrelim  = Nothing
        fastins  = Nothing

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO AST.Module
runJIT mod = do
    withContext $ \context ->
        jit context $ \executionEngine ->
            withModuleFromAST context mod $ \m ->
                withPassManager passes $ \pm -> do
                optmod <- moduleAST m
                s <- moduleLLVMAssembly m
                putStrLn (toStr s)
                EE.withModuleInEngine executionEngine m $ \ee -> do
                    mainfn <- EE.getFunction ee (AST.Name $ toSBS "main")
                    case mainfn of
                      Just fn -> do
                        res <- run fn
                        putStrLn $ "Returned: " ++ show res
                      Nothing -> return ()
                return optmod
