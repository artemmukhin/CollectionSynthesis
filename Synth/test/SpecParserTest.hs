module SpecParserTest where
import CoreLanguage
import SpecParser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

test1 :: Bool
test1 =
  let res = parse query "" "query q(x : Int, y : Bool) = map ($x : Int -> ys) xs" in
  case res of
    Right r -> (r == QueryDef Query {qname = "q", qparams = [Param ("x",Int),Param ("y",Bool)], qbody = App (Map {dom = Param ("x",Int), codom = Unknown, body = Prim (Var "ys")}) (Prim (Var "xs"))})
    Left  e -> False

test2 :: Bool
test2 =
  let res = parse query "" "query q(x : Int, y : Bool) = map ($x : Int -> ys) (filter ($x : Int -> zs) xs)" in
  case res of
    Right r -> (r == QueryDef Query {qname = "q", qparams = [Param ("x",Int),Param ("y",Bool)], qbody = App (Map {dom = Param ("x",Int), codom = Unknown, body = Prim (Var "ys")}) (App (Filter {dom = Param ("x",Int), body = Prim (Var "zs")}) (Prim (Var "xs")))})
    Left  e -> False

test :: Bool
test = test1 && test2
