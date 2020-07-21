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
double = ASTtype.FloatingPointType AST.DoubleFP

-- Functions to refer to references
local ::  AST.Name -> AST.Operand
local = AST.LocalReference double

global ::  AST.Name -> ASTconst.Constant
global = ASTconst.GlobalReference double

-- will refer to toplevel or externally declared fns
externRef :: AST.Name -> AST.Operand
externRef = AST.ConstantOperand . ASTconst.GlobalReference double

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

emptyTopState :: TopState
emptyTopState = TopState "entry" Map.empty [] 1 0 Map.empty

getEmptyTopState :: Cgen a -> TopState
getEmptyTopState (Cgen a) = execState a emptyTopState

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

sortFunctionBlocks :: [(String, FunctionBlock)] -> [(String, FunctionBlock)]
sortFunctionBlocks = sortBy (on compare (Codegen.id . snd))

makeFunctionBlock :: (String, FunctionBlock) -> ASTglobal.BasicBlock
makeFunctionBlock (name, (FunctionBlock st ter _)) = ASTglobal.BasicBlock (AST.Name (toSBS name)) (reverse st) (toterm ter)
    where
        toterm (Just x) = x
        toterm Nothing = error ("Block has no terminator: " ++ (show name))

createFunctionBlocks :: TopState -> [ASTglobal.BasicBlock]
createFunctionBlocks ts = map makeFunctionBlock (sortFunctionBlocks (Map.toList (blockMap ts)))

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

-- each instruction increments this count
incIns :: Cgen Word
incIns = do
    cnt <- gets count
    modify (\s -> s {count = cnt + 1})
    return (cnt+1)

newInstr :: AST.Instruction -> Cgen (AST.Operand)
newInstr instr = do
    cnt <- incIns
    let ref = (AST.UnName cnt)
    blk <- currentFunctionBlock
    let i = stack blk
    modifyFunctionBlock (blk { stack = (ref AST.:= instr) : i })
    return (local ref)

newTerm :: AST.Named AST.Terminator -> Cgen (AST.Named AST.Terminator)
newTerm term = currentFunctionBlock >>= \c -> modifyFunctionBlock c { terminator = Just term } >> return term

-- LLVM monad
newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = AST.defaultModule { AST.moduleName = (toSBS label)}

-- add definition to LLVM module def
addDefinition :: AST.Definition -> LLVM ()
addDefinition d = do
  defs <- gets AST.moduleDefinitions
  modify (\s -> s { AST.moduleDefinitions = defs ++ [d]})

-- define a regular function
defineFn :: String -> [(AST.Type, AST.Name)] -> [ASTglobal.BasicBlock] -> AST.Type -> LLVM ()
defineFn fnname argtypes blocks rettype = addDefinition $
    AST.GlobalDefinition $ ASTglobal.functionDefaults {
        ASTglobal.name        = AST.Name (toSBS fnname)
      , ASTglobal.parameters  = ([ASTglobal.Parameter _type name [] | (_type, name) <- argtypes], False)
      , ASTglobal.returnType  = rettype
      , ASTglobal.basicBlocks = blocks
    }

-- define an extern
externalFn :: String -> [(AST.Type, AST.Name)] -> AST.Type -> LLVM ()
externalFn fnname argtypes rettype = addDefinition $
    AST.GlobalDefinition $ ASTglobal.functionDefaults {
        ASTglobal.name        = AST.Name (toSBS fnname)
      , ASTglobal.linkage     = ASTlink.External
      , ASTglobal.parameters  = ([ASTglobal.Parameter _type name [] | (_type, name) <- argtypes], False)
      , ASTglobal.returnType  = rettype
      , ASTglobal.basicBlocks = []
    }

-- names similar to llvm IR names
-- Arithmetic, contstants (copied functions)
fadd :: AST.Operand -> AST.Operand -> Cgen AST.Operand
fadd a b = newInstr (AST.FAdd AST.noFastMathFlags a b [])

fsub :: AST.Operand -> AST.Operand -> Cgen AST.Operand
fsub a b = newInstr (AST.FSub AST.noFastMathFlags a b [])

fmul :: AST.Operand -> AST.Operand -> Cgen AST.Operand
fmul a b = newInstr (AST.FMul AST.noFastMathFlags a b [])

fdiv :: AST.Operand -> AST.Operand -> Cgen AST.Operand
fdiv a b = newInstr (AST.FDiv AST.noFastMathFlags a b [])

fcmp :: ASTfpp.FloatingPointPredicate -> AST.Operand -> AST.Operand -> Cgen AST.Operand
fcmp cond a b = newInstr $ AST.FCmp cond a b []

cons :: ASTconst.Constant -> AST.Operand
cons = AST.ConstantOperand

uitofp :: AST.Type -> AST.Operand -> Cgen AST.Operand
uitofp ty a = newInstr $ AST.UIToFP a ty []

toArgs :: [AST.Operand] -> [(AST.Operand, [ASTattr.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: AST.Operand -> [AST.Operand] -> Cgen AST.Operand
call fn args = newInstr $ AST.Call Nothing ASTcc.C [] (Right fn) (toArgs args) [] []

alloca :: AST.Type -> Cgen AST.Operand
alloca ty = newInstr $ AST.Alloca ty Nothing 0 []

store :: AST.Operand -> AST.Operand -> Cgen AST.Operand
store ptr val = newInstr $ AST.Store False ptr val Nothing 0 []

load :: AST.Operand -> Cgen AST.Operand
load ptr = newInstr $ AST.Load False ptr Nothing 0 []

-- Control Flow
br :: AST.Name -> Cgen (AST.Named AST.Terminator)
br val = newTerm $ AST.Do $ AST.Br val []

cbr :: AST.Operand -> AST.Name -> AST.Name -> Cgen (AST.Named AST.Terminator)
cbr cond tr fl = newTerm $ AST.Do $ AST.CondBr cond tr fl []

ret :: AST.Operand -> Cgen (AST.Named AST.Terminator)
ret val = newTerm $ AST.Do $ AST.Ret (Just val) []
