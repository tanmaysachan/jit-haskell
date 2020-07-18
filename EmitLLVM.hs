module EmitLLVM where

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import qualified LLVM.Module as Mod
import qualified LLVM.Context as Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as ASTcons
import qualified LLVM.AST.Float as ASTfl
import qualified LLVM.AST.FloatingPointPredicate as ASTfpp

import Codegen
import qualified Syntax
import Utils

-- map all args to type double
modArgs :: [String] -> [(AST.Type, AST.Name)]
modArgs = map (\t -> (double, AST.Name $ toSBS t))

functionsToLLVM :: Syntax.Expr -> LLVM ()
functionsToLLVM (Syntax.Function _name _args _body) = do
    defineFn name args body double
    where
        args = modArgs _args
        name = _name
        body = createFunctionBlocks $ execGen $ do
            entryBlock <- addFunctionBlock "entry"
            setCurrentFunctionBlock entryBlock
            forM args $ \a -> do
                getmem <- alloca double
                store getmem (local (AST.Name a))
                assign a getmem
            indExprGen _body >>= ret
