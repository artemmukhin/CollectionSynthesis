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


newtype Param = Param (String, Type)
instance Show Param where
  show (Param (s, _)) = s


data Func
  = Map { dom :: Param, codom :: Type, body :: Expr }
  | Filter { dom :: Param, body :: Expr }
  | MapFilter { dom :: Param, codom :: Type, bodyMap :: Expr, bodyFilter :: Expr }

showLambda :: Param -> Expr -> String
showLambda d b = "(λ" ++ show d ++ ". " ++ show b ++ ")"

instance Show Func where
  show (Map d _ b) = "map " ++ showLambda d b
  show (Filter d b) = "filter " ++ showLambda d b
  show (MapFilter d _ bm bf) = "mapFilter " ++ showLambda d bm ++ " " ++ showLambda d bf

data PrimExpr
  = Var String
  | IntConst Integer
  | Unary String PrimExpr
  | Binary String PrimExpr PrimExpr

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

instance Show Expr where
  show (Hole t) = "_" ++ show t ++ "_"
  show (Collection name _) = name
  show (App f expr@(App _ _)) = show f ++ " (" ++ show expr ++ ")"
  show (App f expr) = show f ++ " " ++ show expr
  show (Prim expr) =  show expr

typeOf :: Expr -> Type
typeOf (Hole t) = t
typeOf (Collection _ t)  = t
typeOf (App (Map _ cod _) _) = List cod
typeOf (App (Filter _ _) _) = Bool
typeOf (App (MapFilter _ cod _ _) _) = List cod
typeOf (Prim _) = undefined
