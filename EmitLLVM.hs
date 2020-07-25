module EmitLLVM where

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import qualified LLVM.Module as Mod
import qualified LLVM.Context as Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as ASTconst
import qualified LLVM.AST.Float as ASTfl
import qualified LLVM.AST.FloatingPointPredicate as ASTfpp

import Codegen
import qualified Syntax
import Utils
import Jitopt

binops = Map.fromList [
      (Syntax.Plus, fadd)
    , (Syntax.Minus, fsub)
    , (Syntax.Multiply, fmul)
    , (Syntax.Divide, fdiv)
  ]

exprToModule :: Syntax.Expr -> Cgen AST.Operand
exprToModule (Syntax.Float _a) = return (cons (ASTconst.Float (ASTfl.Double _a)))
exprToModule (Syntax.BinOp Syntax.Equals (Syntax.Var _a) _b) = do
    a <- getVar _a
    val <- exprToModule _b
    store a val
    return val
exprToModule (Syntax.BinOp op _a _b) = do
    case Map.lookup op binops of
        Just f -> do
            a <- exprToModule _a
            b <- exprToModule _b
            f a b
        Nothing -> error "No operator found"
exprToModule (Syntax.Var _a) = getVar _a >>= \a -> load a
exprToModule (Syntax.Call _name _args) = do
    mappedargs <- mapM exprToModule _args
    call (externRef (AST.Name (toSBS _name))) mappedargs

-- map all args to type double
modArgs :: [String] -> [(AST.Type, AST.Name)]
modArgs = map (\t -> (double, AST.Name (toSBS t)))

functionsToModule :: Syntax.Expr -> LLVM ()
functionsToModule (Syntax.Function _name _args _body) = do
    defineFn name args body double
    where
        args = modArgs _args
        name = _name
        body = createFunctionBlocks $ getEmptyTopState $ do
            entryBlock <- addFunctionBlock "entry"
            setCurrentFunctionBlock entryBlock
            -- llvm allocate memory and assign operand
            forM _args $ \a -> do
                ptr <- alloca double
                nstore ptr (local (AST.Name $ toSBS a))
                assign a ptr
            exprToModule _body >>= ret
functionsToModule (Syntax.Extern _name _args) = do
    externalFn name args double
    where
        args = modArgs _args
        name = _name
-- just expr
functionsToModule _body = do
    defineFn "main" args body double
    where
        args = []
        body = createFunctionBlocks $ getEmptyTopState $ do
            entryBlock <- addFunctionBlock "entry"
            setCurrentFunctionBlock entryBlock
            exprToModule _body >>= ret

codegen :: AST.Module -> [Syntax.Expr] -> IO AST.Module
codegen mod fns = do
    newast <- runJIT ast
    return newast
    where
        modn = mapM functionsToModule fns
        ast = runLLVM mod modn
