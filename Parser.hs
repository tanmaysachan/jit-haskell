module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as LibExpr
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s f assoc = LibExpr.Infix (reservedOp s >> return (BinOp f)) assoc

-- *,/ and +,- equal precedence, left associative
precendenceTable = [[binary "*" Multiply LibExpr.AssocLeft, binary "/" Divide LibExpr.AssocLeft]
                    ,[binary "+" Plus LibExpr.AssocLeft, binary "-" Minus LibExpr.AssocLeft]
                    ,[binary "=" Equals LibExpr.AssocRight]
                ]

-- treating ints as floats for now
int :: Parser Expr
int = integer >>= \x -> return (Float (fromIntegral x))

floating :: Parser Expr
floating = float >>= \x -> return (Float x)

variable :: Parser Expr
variable = identifier >>= \x -> return (Var x)

function :: Parser Expr
function = do
    reserved "def"
    func <- identifier
    args <- parens (commaSep identifier)
    body <- expr
    return (Function func args body)

extern :: Parser Expr
extern = do
    reserved "extern"
    name <- identifier
    args <- parens (commaSep identifier)
    return (Extern name args)

call :: Parser Expr
call = do
    func <- identifier
    args <- parens (commaSep expr)
    return (Call func args)

expr :: Parser Expr
expr = LibExpr.buildExpressionParser precendenceTable factor

-- order of matching
factor :: Parser Expr
factor = try floating
     <|> try int
     <|> try extern
     <|> try function
     <|> try call
     <|> variable
     <|> parens expr

defn :: Parser Expr
defn = try extern
   <|> try function
   <|> expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseTop :: String -> Either ParseError [Expr]
parseTop s = parse (contents toplevel) "<stdin>" s
