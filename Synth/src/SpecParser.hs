module SpecParser where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.IO
import Control.Monad
import CoreLanguage

{-
query = 'query' name '(' params ')' = expr
param = name ':' type
params = param | param ',' params
expr = func expr | primExpr
func = 'filter' lambda | 'map' lambda
lambda = '(\' name '->' expr ')'
primExpr = name | number | primExpr '+' primExpr | primExpr '*' primExpr
name = String
number = Int
-}


languageDef =
  emptyDef { Token.commentStart     = "/*"
           , Token.commentEnd       = "*/"
           , Token.commentLine      = "//"
           , Token.identStart       = letter
           , Token.identLetter      = alphaNum
           , Token.reservedNames    = [ "if"
                                      , "then"
                                      , "else"
                                      , "filter"
                                      , "map"
                                      , "query"
                                      , "true"
                                      , "false"
                                      , "not"
                                      , "and"
                                      , "or"
                                      , "Int"
                                      , "Bool"
                                      ]
           , Token.reservedOpNames  = [ "+", "-", "*", "/", "=", "==", "%", "->",
                                        ":", "<", ">", "and", "or", "not", "$"]
           }


lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
comma      = Token.comma      lexer
whiteSpace = Token.whiteSpace lexer

specParser :: Parser Spec
specParser = whiteSpace >> (sepBy1 declaration whiteSpace)

declaration :: Parser Declaration
declaration = query <|> collection


query :: Parser Declaration
query = do
  reserved "query"
  spaces
  name <- identifier
  char '('
  parameters <- params
  char ')'
  spaces
  reservedOp "="
  spaces
  expression <- expr
  return $ QueryDef Query { qname = name, qparams = parameters, qbody = expression }

params :: Parser Params
params = sepBy1 param comma

param :: Parser Param
param = do
  name <- identifier
  spaces
  reservedOp ":"
  spaces
  t <- primType
  return $ Param (name, t)

primType :: Parser Type
primType = intType <|> boolType

intType :: Parser Type
intType = do
  reserved "Int"
  return Int

boolType :: Parser Type
boolType = do
  reserved "Bool"
  return Bool

expr :: Parser Expr
expr = funcExpr <|> primExpr <|> parens expr

funcExpr :: Parser Expr
funcExpr = do
  f <- func
  e <- expr
  return $ App f e

func :: Parser Func
func = filterFunc <|> mapFunc

filterFunc :: Parser Func
filterFunc = do
  reserved "filter"
  lam <- lambda
  return Filter { dom = lparam lam, body = lexpr lam }

mapFunc :: Parser Func
mapFunc = do
  reserved "map"
  lam <- lambda
  return Map { dom = lparam lam, codom = Int, body = lexpr lam }

-- TODO
lambda :: Parser Lambda
lambda = do
  reserved "("
  reservedOp "$"
  parameter <- param
  spaces
  reserved "->"
  spaces
  expression <- expr
  reserved ")"
  return Lambda { lparam = parameter, lexpr = expression }

primExpr :: Parser Expr
primExpr = do
  ident <- identifier
  return $ Collection ident Int

collection :: Parser Declaration
collection = do
  name <- identifier
  spaces
  reservedOp ":"
  spaces
  t <- primType
  return $ CollectionDef (Collection name t)
