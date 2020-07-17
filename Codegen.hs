{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import Data.ByteString.Short
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as ASTglobal
import qualified LLVM.AST.Linkage as ASTlink
import qualified LLVM.AST.Attribute as ASTattr
import qualified LLVM.AST.Constant as ASTconst
import qualified LLVM.AST.FloatingPointPredicate as ASTfpp
import qualified LLVM.AST.CallingConvention as ASTcc
import qualified LLVM.AST.Type as ASTtype

import Utils

double :: AST.Type
double = ASTtype.double

data FunctionBlock
    = FunctionBlock {
        stack :: [AST.Named AST.Instruction]
      , terminator :: Maybe(AST.Named AST.Terminator)
      , id :: Int
    } deriving (Show)

type NameBlockMap = Map.Map String Int

data TopState
    = TopState {
        curBlock :: String
      , blockMap :: Map.Map String FunctionBlock
      , symTable :: [(String, AST.Operand)]
      , blockCount :: Int
      , count :: Word
      , blockNames :: NameBlockMap
    }

-- Stores state of generation
newtype Cgen a = Cgen (State TopState a)
  deriving (Functor, Applicative, Monad, MonadState TopState)

-- Function Block functions
emptyFunctionBlock :: Int -> FunctionBlock
emptyFunctionBlock _id = FunctionBlock {
                            stack = []
                          , terminator = Nothing
                          , Codegen.id = _id
                        }

entryFunctionBlock :: Cgen String
entryFunctionBlock = gets curBlock

-- scam function
getUniqueName :: String -> NameBlockMap -> (String, NameBlockMap)
getUniqueName name nmap =
    case Map.lookup name nmap of
        Nothing -> (name, Map.insert name 1 nmap)
        Just ix -> (name ++ show ix, Map.insert name (ix+1) nmap)

addFunctionBlock :: String -> Cgen String
addFunctionBlock blockName = do
    _blockMap <- gets blockMap
    _blockCount <- gets blockCount
    _blockNames <- gets blockNames

    let newBlock = emptyFunctionBlock _blockCount
    let (newBlockName, newBlockMap) = getUniqueName blockName _blockNames

    modify $ \state -> state {
                            blockCount = _blockCount + 1
                          , blockNames = newBlockMap
                          , blockMap = Map.insert newBlockName newBlock _blockMap
                        }
    return newBlockName

setCurrentFunctionBlock :: String -> Cgen String
setCurrentFunctionBlock n = modify (\s -> s {curBlock = n}) >> return n

getCurrentFunctionBlock :: Cgen String
getCurrentFunctionBlock = gets curBlock

-- TODO: idk if this works
modifyFunctionBlock :: FunctionBlock -> Cgen ()
modifyFunctionBlock blk = modify $ \s -> s {blockMap = Map.insert (curBlock s) blk (blockMap s)}

currentFunctionBlock :: Cgen FunctionBlock
currentFunctionBlock = do
    cur <- gets curBlock
    blocks <- gets blockMap
    case Map.lookup cur blocks of
        Nothing -> error ("No block with name: " ++ show cur)
        Just x -> return x

-- symbol table functions
assign :: String -> AST.Operand -> Cgen ()
assign n op = gets symTable >>= \t -> modify $ \s -> s {symTable = [(n, op)] ++ t}

getVar :: String -> Cgen AST.Operand
getVar name = do
    symtab <- gets symTable
    case lookup name symtab of
        Nothing -> error ("Variable not in scope: " ++ show name)
        Just op -> return op

-- LLVM monad
newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: ShortByteString -> AST.Module
emptyModule label = AST.defaultModule { AST.moduleName = label }

-- add definition to LLVM module def
addDefinition :: AST.Definition -> LLVM ()
addDefinition d = do
  defs <- gets AST.moduleDefinitions
  modify (\s -> s { AST.moduleDefinitions = defs ++ [d]})

-- define a regular function
defineFn :: String -> [(AST.Type, String)] -> [ASTglobal.BasicBlock] -> AST.Type -> LLVM ()
defineFn fnname argtypes blocks rettype = addDefinition $
    AST.GlobalDefinition $ ASTglobal.functionDefaults {
        ASTglobal.name        = AST.Name (toSBS fnname)
      , ASTglobal.parameters  = ([ASTglobal.Parameter _type (AST.Name (toSBS name)) [] | (_type, name) <- argtypes], False)
      , ASTglobal.returnType  = rettype
      , ASTglobal.basicBlocks = blocks
    }

-- define an extern
externalFn :: String -> [(AST.Type, String)] -> AST.Type -> LLVM ()
externalFn fnname argtypes rettype = addDefinition $
    AST.GlobalDefinition $ ASTglobal.functionDefaults {
        ASTglobal.name        = AST.Name (toSBS fnname)
      , ASTglobal.linkage     = ASTlink.External
      , ASTglobal.parameters  = ([ASTglobal.Parameter _type (AST.Name (toSBS name)) [] | (_type, name) <- argtypes], False)
      , ASTglobal.returnType  = rettype
      , ASTglobal.basicBlocks = []
    }
