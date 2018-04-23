module SpecParserTest where
import CoreLanguage
import SpecParser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language


test :: Bool
test =
  let test1 = parse specParser "" "query q(x : Int, y : Bool) = map ($x : Int -> ys) xs" in
  case test1 of
    Right r -> (r == Query {qname = "q", qparams = [Param ("x",Int),Param ("y",Bool)], qbody = App (Map {dom = Param ("x",Int), codom = Int, body = Collection "ys" Int}) (Collection "xs" Int)})
    Left  e -> False
