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
                                        ":", "<", ">", "and", "or", "not"]
           }

type Params = [Param]
data Query = Query { qname :: String, qparams :: Params, qbody :: Expr }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
comma      = Token.comma      lexer
whiteSpace = Token.whiteSpace lexer

specParser :: Parser Query
specParser = whiteSpace >> query

query :: Parser Query
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
  return Query {}

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
expr = funcExpr <|> primExpr

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
  b <- lambda
  return Filter { dom = ("x", Int), body = b }

-- TODO
lambda = identifier

mapFunc :: Parser Func
mapFunc = do
  reserved "map"
  b <- lambda
  return Map { dom = ("x", Int), codom = Int, body = b }

primExpr :: Parser Expr
primExpr = collection

collection :: Expr
collection = do
  name <- identifier
  return Collection name Int
