module SpecParserTest where
import CoreLanguage
import SpecParser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

test1 :: Bool
test1 =
  let res = parse specParser "" "query q(x : Int, y : Bool) = map ($x : Int -> ys) xs" in
  case res of
    Right r -> (r == Query {qname = "q", qparams = [Param ("x",Int),Param ("y",Bool)], qbody = App (Map {dom = Param ("x",Int), codom = Int, body = Collection "ys" Int}) (Collection "xs" Int)})
    Left  e -> False

test2 :: Bool
test2 =
  let res = parse specParser "" "query q(x : Int, y : Bool) = map ($x : Int -> ys) (filter ($x : Int -> zs) xs)" in
  case res of
    Right r -> (r == Query {qname = "q", qparams = [Param ("x",Int),Param ("y",Bool)], qbody = App (Map {dom = Param ("x",Int), codom = Int, body = Collection "ys" Int}) (App (Filter {dom = Param ("x",Int), body = Collection "zs" Int}) (Collection "xs" Int))})
    Left  e -> False
