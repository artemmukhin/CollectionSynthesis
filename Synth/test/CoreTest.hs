module Test where
import CoreLanguage

xsExpr :: Expr
xsExpr = Prim $ Var "xs"
ysExpr :: Expr
ysExpr = Prim $ Var "ys"
xssExpr :: Expr
xssExpr = Prim $ Var "xss"

expr1 :: Expr
expr1 = Prim (Binary "+" (Unary "head" (Var "xs")) (IntConst 42))

func :: Func
func = Map{ dom = Param ("xs", List Int), codom = Int, body = expr1 }

f :: PrimExpr
f = Binary "+" (Var "x") (IntConst 1)
p :: PrimExpr
p = Binary "==" (Binary "%" (Var "x") (IntConst 2)) (IntConst 0)
p' :: PrimExpr
p' = Binary "<" (Binary "*" (Var "x") (IntConst 3)) (IntConst 10)
p'' :: PrimExpr
p'' = Binary ">" (Binary "+" (Var "x") (IntConst 5)) (IntConst 7)


expr2 :: Expr
expr2 = App func xssExpr
-- expr2 ~ map (\xs -> head xs + 42) xss

expr3 :: Expr
expr3 = App Map{ dom = Param ("x", Int), codom = Int, body = fterm } xsExpr where
  fterm = App Filter{ dom = Param ("y", Int), body = Prim (Binary "==" (Binary "%" (Var "x") (Var "y")) (IntConst 0)) } ysExpr
-- expr3 = map (\x. filter (\y. x % y == 0) ys) xs

expr4 :: Expr
expr4 = App MapFilter{ dom = Param ("x", Int), codom = Int, bodyFilter = Prim p, bodyMap = Prim f } xsExpr
-- expr4 = mapFilter (\x. x + 1) (\x. x % 2 == 0) xs

expr5 :: Expr
expr5 = App Map{ dom = Param ("x", Int), codom = Int, body = Prim f } e_filter where
  e_filter = App Filter{ dom = Param ("x", Int), body = Prim p } xsExpr
-- expr5 = map (λx. x + 1) (filter (λx. x % 2 == 0) xs)

expr6 :: Expr
expr6 = App Map{ dom = Param ("x", Int), codom = Int, body = Prim f } e_filter where
  e_filter = App Filter{ dom = Param ("x", Int), body = Prim p } e_filter'
  e_filter' = App Filter{ dom = Param ("x", Int), body = Prim p' } xsExpr
-- expr6 = map (λx. x + 1) (filter (λx. x % 2 == 0) (filter (λx. x * 3 < 10) xs))

expr7 :: Expr
expr7 = App MapFilter{ dom = Param ("x", Int), codom = Int, bodyFilter = Prim (Binary "and" p p'), bodyMap = Prim f } xsExpr
-- expr7 = mapFilter (λx. x % 2 == 0 and x * 3 < 10) (λx. x + 1) xs

expr8 :: Expr
expr8 = App Filter{ dom = Param ("x", Int), body = Prim p } e_filter where
  e_filter = App Filter{ dom = Param ("x", Int), body = Prim p' } e_filter'
  e_filter' = App Filter{ dom = Param ("x", Int), body = Prim p'' } xsExpr
-- expr8 = filter (λx. x % 2 == 0) (filter (λx. x * 3 < 10) (filter (λx. x + 5 > 7) xs))

expr9 :: Expr
expr9 = App Filter{ dom = Param ("x", Int), body = b } xsExpr where
  b = Prim (Binary "and" (Binary "and" p p') p'')

{-
testTypes :: Bool
testTypes =
  typeOf xssExpr == List (List Int) &&
  typeOf expr2 == List Int &&
  typeOf expr3 == List Int &&
  typeOf expr4 == List Int &&
  typeOf expr5 == List Int
-}

testPretty :: Bool
testPretty =
  pretty expr2 == "map (λxs. head xs + 42) xss" &&
  pretty expr3 == "map (λx. filter (λy. x % y == 0) ys) xs" &&
  pretty expr4 == "mapFilter (λx. x % 2 == 0) (λx. x + 1) xs" &&
  pretty expr5 == "map (λx. x + 1) (filter (λx. x % 2 == 0) xs)" &&
  pretty expr6 == "map (λx. x + 1) (filter (λx. x % 2 == 0) (filter (λx. x * 3 < 10) xs))" &&
  pretty expr7 == "mapFilter (λx. x % 2 == 0 and x * 3 < 10) (λx. x + 1) xs" &&
  pretty expr8 == "filter (λx. x % 2 == 0) (filter (λx. x * 3 < 10) (filter (λx. x + 5 > 7) xs))" &&
  pretty expr9 == "filter (λx. x % 2 == 0 and x * 3 < 10 and x + 5 > 7) xs"

testReduce :: Bool
testReduce =
  reduce expr1 == expr1 &&
  reduce expr2 == expr2 &&
  reduce expr3 == expr3 &&
  reduce expr4 == expr4 &&
  reduce expr5 == expr4 &&
  reduce expr6 == expr7 &&
  reduce expr8 == expr9

test :: Bool
test = {- testTypes && -} testPretty && testReduce

main :: IO ()
main =  putStrLn $ if test then "Tests completed successfully!" else "Tests failure"
