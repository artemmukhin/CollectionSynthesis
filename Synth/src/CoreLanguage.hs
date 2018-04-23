{-# LANGUAGE TypeSynonymInstances #-}
module CoreLanguage where

class Pretty a where
  pretty :: a -> String

data Type
  = Int
  | Bool
  | List Type
  deriving (Eq, Show)

instance Pretty Type where
  pretty Int      = "Int"
  pretty Bool     = "Bool"
  pretty (List t) = "[" ++ pretty t ++ "]"


newtype Param = Param (String, Type) deriving (Eq, Show)
instance Pretty Param where
  pretty (Param (s, _)) = s


data Func
  = Map { dom :: Param, codom :: Type, body :: Expr }
  | Filter { dom :: Param, body :: Expr }
  | MapFilter { dom :: Param, codom :: Type, bodyFilter :: Expr, bodyMap :: Expr }
  deriving (Eq, Show)

prettyLambda :: Param -> Expr -> String
prettyLambda d b = "(λ" ++ pretty d ++ ". " ++ pretty b ++ ")"

instance Pretty Func where
  pretty (Map d _ b) = "map " ++ prettyLambda d b
  pretty (Filter d b) = "filter " ++ prettyLambda d b
  pretty (MapFilter d _ bf bm) = "mapFilter " ++ prettyLambda d bf ++ " " ++ prettyLambda d bm


data PrimExpr
  = Var String
  | IntConst Integer
  | Unary String PrimExpr
  | Binary String PrimExpr PrimExpr
  deriving (Eq, Show)

instance Pretty PrimExpr where
  pretty (Var s) = s
  pretty (IntConst n) = show n
  pretty (Unary op e) = op ++ " " ++ pretty e
  pretty (Binary op e1 e2)  = pretty e1 ++ " " ++ op ++ " " ++ pretty e2


data Expr
  = Hole Type
  | Collection String Type
  | App Func Expr
  | Prim PrimExpr
  | Compose Expr Expr
  deriving (Eq, Show)

instance Pretty Expr where
  pretty (Hole t) = "_" ++ pretty t ++ "_"
  pretty (Collection name _) = name
  pretty (App f expr@(App _ _)) = pretty f ++ " (" ++ pretty expr ++ ")"
  pretty (App f expr) = pretty f ++ " " ++ pretty expr
  pretty (Prim expr) =  pretty expr
  pretty (Compose e1 e2) = "((" ++ pretty e1 ++ ") . (" ++ pretty e2 ++ "))"


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

reduceStep (App (Filter _ (Prim p)) (App (Filter d (Prim p')) xs)) = (Incomplete, App (Filter d (Prim (Binary "and" p p'))) xs)
reduceStep (App (Filter _ p) (App (Filter d p') xs)) = (Incomplete, App (Filter d (Compose p p')) xs)

reduceStep (App (Filter _ p) (App (Map d c f) xs)) = (Incomplete, App (MapFilter d c (Compose p f) f) xs)

reduceStep (App (Filter _ (Prim p)) (App (MapFilter d c (Prim p') f) xs)) = (Incomplete, App (MapFilter d c (Prim (Binary "and" p p')) f) xs)
reduceStep (App (Filter _ p) (App (MapFilter d c p' f) xs)) = (Incomplete, App (MapFilter d c (Compose p p') f) xs)

reduceStep e@(App Filter{} _) = (Complete, e)
reduceStep (App (MapFilter _ c p f) (App (MapFilter d' _ p' f') xs)) = (Incomplete, App (MapFilter d' c (Compose p' (Compose p f')) (Compose f f')) xs)

reduceStep (App (MapFilter _ c (Prim p) f) (App (Filter d' (Prim p')) xs)) = (Incomplete, App (MapFilter d' c (Prim (Binary "and" p p')) f) xs)
reduceStep (App (MapFilter _ c p f) (App (Filter d' p') xs)) = (Incomplete, App (MapFilter d' c (Compose p p') f) xs)

reduceStep (App (MapFilter _ c p f) (App (Map d' _ f') xs)) = (Incomplete, App (MapFilter d' c (Compose p f') (Compose f f')) xs)
reduceStep e@(App MapFilter{} _) = (Complete, e)
reduceStep e@(Compose _ _) = (Complete, e)

reduce :: Expr -> Expr
reduce expr = snd $ go (Incomplete, expr) where
  go (Incomplete, e) = go (reduceStep e)
  go (Complete, e)   = (Complete, e)
