module Main where

data FuncTtoInt_ = FuncTtoInt_ deriving(Show)
data FuncTtoBool_ = FuncTtoBool_ deriving(Show)
data FuncTtoT_ = FuncTtoT_ deriving(Show)
data List_ = List_ deriving(Show)
data T_ = T_ deriving (Show)


data Expr
  = IntExpr
  | ListExpr
  | TExpr
  | MyListExpr
  | MySetExpr
  | MyMapExpr
  deriving(Show)

data ListExpr
  = MyList_content MyListExpr
  | MySet_content MySetExpr
  deriving(Show)

data TExpr
  = MyList_head MyListExpr
  | MyList_last MyListExpr
  | MyList_maximize FuncTtoInt_ MyListExpr
  | MyList_find FuncTtoBool_ MyListExpr

  | MySet_find T_ MySetExpr

  | MyMap_get T_ MyMapExpr
  deriving(Show)

data MyListExpr
  = MyList_build List_
  | MyList_tail MyListExpr
  | MyList_map FuncTtoT_ MyListExpr
  | MyList_filter FuncTtoBool_ MyListExpr
  | MyList_concat MyListExpr MyListExpr
  deriving(Show)

data MySetExpr
  = MySet_build List_
  | MySet_add T_ MySetExpr
  | MySet_filter FuncTtoBool_ MySetExpr
  | MySet_union MySetExpr MySetExpr
  deriving(Show)

data MyMapExpr
  = MyMap_build List_
  | MyMap_add T_ T_ MyMapExpr
  deriving(Show)

data IntExpr
  = MyList_size MyListExpr
  | MySet_size MySetExpr
  | MyMap_size MyMapExpr
  deriving(Show)

class ExprGenerator coll where
  funcBuild             :: coll
  funcsCollToColl       :: [coll -> coll]
  funcsColl2ToColl      :: [coll -> coll -> coll]
  funcsCollToList       :: [coll -> ListExpr]
  funcsCollToInt        :: [coll -> IntExpr]
  funcsCollToT          :: [coll -> TExpr]

  generate :: [coll] -> [coll]
  generate []       = generate [funcBuild]
  generate current  = current ++ generate next where
      next = concatMap addUnary current ++ concatMap addBinary (cartProd current current)
      cartProd xs ys = [(x,y) | x <- xs, y <- ys]

  addUnary :: coll -> [coll]
  addUnary expr = map (\f -> f expr) funcsCollToColl

  addBinary :: (coll, coll) -> [coll]
  addBinary (e1,e2) = concatMap (\f -> [f e1 e2, f e2 e1]) funcsColl2ToColl


instance ExprGenerator MyListExpr where
  funcBuild        = MyList_build List_
  funcsCollToColl  = [MyList_tail, MyList_map FuncTtoT_, MyList_filter FuncTtoBool_]
  funcsColl2ToColl = [MyList_concat]
  funcsCollToList  = [MyList_content]
  funcsCollToInt   = [MyList_size]
  funcsCollToT     = [MyList_head, MyList_last, MyList_maximize FuncTtoInt_, MyList_find FuncTtoBool_]

instance ExprGenerator MySetExpr where
  funcBuild        = MySet_build List_
  funcsCollToColl  = [MySet_add T_, MySet_filter FuncTtoBool_]
  funcsColl2ToColl = [MySet_union]
  funcsCollToList  = [MySet_content]
  funcsCollToInt   = [MySet_size]
  funcsCollToT     = [MySet_find T_]

instance ExprGenerator MyMapExpr where
  funcBuild        = MyMap_build List_
  funcsCollToColl  = [MyMap_add T_ T_]
  funcsColl2ToColl = []
  funcsCollToList  = []
  funcsCollToInt   = [MyMap_size]
  funcsCollToT     = [MyMap_get T_]


myListExprs = generate [] :: [MyListExpr]
listExprs   = [f e | e <- myListExprs, f <- funcsCollToColl]
tExprs      = [f e | e <- myListExprs, f <- funcsCollToT]
intExprs    = [f e | e <- myListExprs, f <- funcsCollToInt]


main :: IO ()
main = putStr ""
