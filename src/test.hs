module Main where
import Data.Map(Map, fromList, lookup)
import Data.Maybe(fromMaybe)

data PrimType
  = Int
  | Bool
  | Poly String
  deriving (Show, Eq)

data TypeConstructor
  = MyList
  | MyMap
  | MySet
  deriving (Show, Eq)

data Type
  = Prim PrimType
  | List PrimType
  | Func [Type] Type
  | Constructor TypeConstructor [Type]
  deriving (Show, Eq)

data Identifier = Identifier String
instance Show Identifier where show (Identifier s) = s
data Function   = Function { name :: String, domain :: [Type], range :: Type }

instance Show Function where
  show = name

data Term
  = Hole PrimType
  | HoleList PrimType
  | Var Identifier Type
  | Abstraction [Identifier] Term
  | Application Function [Term]

instance Show Term where
  show (Hole pt)     = "_" ++ show pt ++ "_"
  show (HoleList pt) = "[_" ++ show pt ++ "_]"
  show (Var ident t) = show ident ++ ": " ++ show t
  show (Abstraction args term) = "(λ " ++ concatMap show args ++ ". " ++ show term ++ ")"
  show (Application f args) = show f ++ " " ++ unwords (map (\a -> "(" ++ show a ++ ")") args)

functions :: [Function]
functions = [Function { name    = "List.build",
                        domain  = [List (Poly "t")],
                        range   = Constructor MyList [Prim (Poly "t")] },
             Function { name    = "List.map",
                        domain   = [Func [Prim (Poly "t")] (Prim (Poly "t")), Constructor MyList [Prim (Poly "t")]],
                        range    = Constructor MyList [Prim (Poly "t")] },
             Function { name    = "List.concat",
                        domain   = [Constructor MyList [Prim (Poly "t")], Constructor MyList [Prim (Poly "t")]],
                        range    = Constructor MyList [Prim (Poly "t")] }
            ]

functionsMap :: Data.Map.Map String Function
functionsMap = Data.Map.fromList $ zip (map name functions) functions

getFunction :: String -> Function
getFunction funcname = fromMaybe (error "No such function") f
  where f = Data.Map.lookup funcname functionsMap

functionsByRange :: Type -> [Function]
functionsByRange r = filter (\f -> range f == r) functions

-- todo: type unification
functionsByTypeConstr :: TypeConstructor -> [Type] -> [Function]
functionsByTypeConstr constr types =
  filter (\f -> case range f of
    Constructor c _ -> c == constr
    _ -> False) functions

freshvar :: [Term] -> Type -> Identifier
freshvar env _ = Identifier ("a" ++ show i) where
  i = length $ filter (\t -> case t of Var _ _ -> True; _ -> False) env


generate :: [Term] -> Type -> [Term]
generate _ (Prim t) = [Hole t]
generate _ (List t) = [HoleList t]
generate env (Func dom ran) = [Abstraction vars body | body <- bodies] where
  vars = map (freshvar env) dom
  bodies = generate (env ++ zipWith Var vars dom) ran
generate env (Constructor constr types) =
  [Application f args | f <- functionsByTypeConstr constr types,
                        args <- sequence [generate env t | t <- domain f]]


main :: IO ()
main = putStr ""
