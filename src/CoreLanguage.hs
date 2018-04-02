module CoreLanguage where

data Type
  = Int
  | List Type
  deriving (Eq)

instance Show Type where
  show Int      = "Int"
  show (List t) = "[" ++ show t ++ "]"

newtype Id = Id String
instance Show Id where show (Id s) = s

data Func
  = Map     Type Type FuncTerm
  | Filter  Type Type FuncTerm

data FuncTerm
  = Var String
  -- Int
  | IntConst Integer
  | Plus FuncTerm FuncTerm
  | Mul FuncTerm FuncTerm
  -- List
  | Length FuncTerm
  | Concat FuncTerm FuncTerm
  | Head FuncTerm

data Expr
  = Hole Type
  | Collection Id Type
  | App Func Expr

typeOf :: Expr -> Type
typeOf (Hole t) = t
typeOf (Collection _ t)  = t
typeOf (App (Map _ b _) _) = b
typeOf (App (Filter _ b _) _) = b
