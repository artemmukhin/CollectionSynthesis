{-# LANGUAGE TypeSynonymInstances #-}
module CoreLanguage where

data Type
  = Int
  | Bool
  | List Type
  deriving (Eq)

instance Show Type where
  show Int      = "Int"
  show Bool     = "Bool"
  show (List t) = "[" ++ show t ++ "]"


newtype Param = Param (String, Type) deriving (Eq)
instance Show Param where
  show (Param (s, _)) = s


data Func
  = Map { dom :: Param, codom :: Type, body :: Expr }
  | Filter { dom :: Param, body :: Expr }
  | MapFilter { dom :: Param, codom :: Type, bodyFilter :: Expr, bodyMap :: Expr }
  deriving (Eq)

showLambda :: Param -> Expr -> String
showLambda d b = "(λ" ++ show d ++ ". " ++ show b ++ ")"

instance Show Func where
  show (Map d _ b) = "map " ++ showLambda d b
  show (Filter d b) = "filter " ++ showLambda d b
  show (MapFilter d _ bf bm) = "mapFilter " ++ showLambda d bf ++ " " ++ showLambda d bm


data PrimExpr
  = Var String
  | IntConst Integer
  | Unary String PrimExpr
  | Binary String PrimExpr PrimExpr
  deriving (Eq)

instance Show PrimExpr where
  show (Var s) = s
  show (IntConst n) = show n
  show (Unary op e) = op ++ " " ++ show e
  show (Binary op e1 e2)  = show e1 ++ " " ++ op ++ " " ++ show e2


data Expr
  = Hole Type
  | Collection String Type
  | App Func Expr
  | Prim PrimExpr
  | Compose Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Hole t) = "_" ++ show t ++ "_"
  show (Collection name _) = name
  show (App f expr@(App _ _)) = show f ++ " (" ++ show expr ++ ")"
  show (App f expr) = show f ++ " " ++ show expr
  show (Prim expr) =  show expr
  show (Compose e1 e2) = "((" ++ show e1 ++ ") . (" ++ show e2 ++ "))"


typeOf :: Expr -> Type
typeOf (Hole t) = t
typeOf (Collection _ t)  = t
typeOf (App (Map _ cod _) _) = List cod
typeOf (App (Filter _ _) _) = Bool
typeOf (App (MapFilter _ cod _ _) _) = List cod
typeOf (Prim _) = undefined
typeOf (Compose e _) = typeOf e

{-
filter p (map f xs)           =  mapFilter (p . f) f xs  =  map (\x -> if p (f x) then f x else None)
map f (filter p xs)           =  mapFilter p     f xs  =  map (\x -> if p x then f x else None)
filter p' (mapFilter p f xs)  =  mapFilter (\x -> p x && p' x) f xs
map f' (mapFilter p f xs)     =  mapFilter p (f' . f) xs
mapFilter p f (filter p' xs)  =  mapFilter (\x -> p x && p' x) f xs
mapFilter p f (map f' xs)     =  mapFilter (p . f') (f . f') xs
-}

data ReduceState = Complete | Incomplete deriving (Eq, Show)

reduceStep :: Expr -> (ReduceState, Expr)
reduceStep e@(Hole _) = (Complete, e)
reduceStep e@(Collection _ _) = (Complete, e)
reduceStep e@(Prim _) = (Complete, e)
reduceStep (App (Map _ c f) (App (Map d _ f') xs)) = (Incomplete, App (Map d c (Compose f f')) xs)
reduceStep (App (Map _ c f) (App (Filter d p) xs)) = (Incomplete, App (MapFilter d c p f) xs)
reduceStep (App (Map _ c f) (App (MapFilter d _ p f') xs)) = (Incomplete, App (MapFilter d c p (Compose f f')) xs)
reduceStep e@(App Map{} _) = (Complete, e)
reduceStep (App (Filter _ p) (App (Filter d p') xs)) = (Incomplete, App (Filter d (Compose p p')) xs)
reduceStep (App (Filter _ p) (App (Map d c f) xs)) = (Incomplete, App (MapFilter d c (Compose p f) f) xs)
reduceStep (App (Filter _ p) (App (MapFilter d c p' f) xs)) = (Incomplete, App (MapFilter d c (Compose p p') f) xs)
reduceStep e@(App Filter{} _) = (Complete, e)
reduceStep (App (MapFilter _ c p f) (App (MapFilter d' _ p' f') xs)) = (Incomplete, App (MapFilter d' c (Compose p' (Compose p f')) (Compose f f')) xs)
reduceStep (App (MapFilter _ c p f) (App (Filter d' p') xs)) = (Incomplete, App (MapFilter d' c (Compose p p') f) xs)
reduceStep (App (MapFilter _ c p f) (App (Map d' _ f') xs)) = (Incomplete, App (MapFilter d' c (Compose p f') (Compose f f')) xs)
reduceStep e@(App MapFilter{} _) = (Complete, e)
reduceStep e@(Compose _ _) = (Complete, e)

reduce :: Expr -> Expr
reduce expr = snd $ go (Incomplete, expr) where
  go (Incomplete, e) = go (reduceStep e)
  go (Complete, e)   = (Complete, e)
