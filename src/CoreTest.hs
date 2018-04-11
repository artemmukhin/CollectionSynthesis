module Test where
import CoreLanguage

funcTerm :: Expr
funcTerm = Prim (Binary "+" (Unary "head" (Var "xs")) (IntConst 42))

func :: Func
func = Map{ dom = Param ("xs", List Int), codom = Int, body = funcTerm }

expr1 :: Expr
expr1 = Collection "xss" (List (List Int))

expr2 :: Expr
expr2 = App func expr1
-- expr2 ~ map (\xs -> head xs + 42) xss

expr3 :: Expr
expr3 = App Map{ dom = Param ("x", Int), codom = Int, body = fterm } e_xs where
  e_xs = Collection "xs" (List Int)
  e_ys = Collection "ys" (List Int)
  fterm = App Filter{ dom = Param ("y", Int), body = Prim (Binary "==" (Binary "%" (Var "x") (Var "y")) (IntConst 0)) } e_ys
-- expr3 = map (\x. filter (\y. x % y == 0) ys) xs

expr4 :: Expr
expr4 = App MapFilter{ dom = Param ("x", Int), codom = Int, bodyMap = mbody, bodyFilter = fbody } e_xs where
  e_xs = Collection "xs" (List Int)
  mbody = Prim (Binary "+" (Var "x") (IntConst 1))
  fbody = Prim (Binary "==" (Binary "%" (Var "x") (IntConst 2)) (IntConst 0))
-- expr4 = mapFilter (\x. x + 1) (\x. x % 2 == 0) xs



testTypes :: Bool
testTypes =
  typeOf expr1 == List (List Int) &&
  typeOf expr2 == List Int &&
  typeOf expr3 == List Int

testShow :: Bool
testShow =
  show expr2 == "map (λxs. head xs + 42) xss" &&
  show expr3 == "map (λx. filter (λy. x % y == 0) ys) xs" &&
  show expr4 == "mapFilter (λx. x + 1) (λx. x % 2 == 0) xs"

test :: Bool
test = testTypes && testShow
