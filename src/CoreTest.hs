module Test where
import CoreLanguage

funcTerm :: FuncTerm
funcTerm = Plus (Head (Var "xs")) (IntConst 42)

func :: Func
func = Map (List Int) Int funcTerm

expr1 :: Expr
expr1 = Collection (Id "xss") (List (List Int))

expr2 :: Expr
expr2 = App func expr1
-- expr ~ map (\xs -> head xs + 42) xss
