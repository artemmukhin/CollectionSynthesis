module Main where
import           Data.List
import qualified Data.Map.Lazy as Map
import           Data.Maybe (fromMaybe, mapMaybe)


data PrimType
  = Int
  | Bool
  | Poly String
  deriving (Eq)

instance Show PrimType where
  show Int      = "Int"
  show Bool     = "Bool"
  show (Poly t) = t

data TypeConstructor
  = MyList
  | MyMap
  | MySet
  deriving (Show, Eq)

data Type
  = Prim PrimType
  | List Type
  | Func [Type] Type
  | Constructor TypeConstructor [Type]
  deriving (Eq)

instance Show Type where
  show (Prim pt)      = show pt
  show (List pt)      = "[" ++ show pt ++ "]"
  show (Func dom ran) = "(" ++ unwords (map (\t -> show t ++ "→") dom) ++ show ran ++ ")"
  show (Constructor tconstr types) = show tconstr ++ "<" ++ unwords (map show types) ++ ">"

data Identifier = Identifier String
instance Show Identifier where show (Identifier s) = s

data Function = Function { name :: String, domain :: [Type], range :: Type }
instance Show Function where show = name
instance Eq   Function where (==) f1 f2 = name f1 == name f2
instance Ord  Function where (<=) f1 f2  = name f1 <= name f2

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
  show (Abstraction args term) = "(λ " ++ unwords (map show args) ++ " → " ++ show term ++ ")"
  show (Application f args) = show f ++ " " ++ unwords (map (\a -> "(" ++ show a ++ ")") args)

functions :: [Function]
functions = [Function { name    = "List.build",
                        domain  = [List (Prim (Poly "t"))],
                        range   = Constructor MyList [Prim (Poly "t")] },
             Function { name    = "List.map",
                        domain   = [Func [Prim (Poly "t")] (Prim (Poly "t")), Constructor MyList [Prim (Poly "t")]],
                        range    = Constructor MyList [Prim (Poly "t")] },
             Function { name    = "List.concat",
                        domain   = [Constructor MyList [Prim (Poly "t")], Constructor MyList [Prim (Poly "t")]],
                        range    = Constructor MyList [Prim (Poly "t")] },
             Function { name    = "List.content",
                        domain   = [Constructor MyList [Prim (Poly "t")]],
                        range    = List (Prim (Poly "t")) }
            ]

functionsMap :: Map.Map String Function
functionsMap = Map.fromList $ zip (map name functions) functions

getFunction :: String -> Function
getFunction funcname = fromMaybe (error "No such function") f
  where f = Map.lookup funcname functionsMap

data Subst = Subst String Type deriving (Show)

{-
 Example:
 unify (Constructor MyList [Constructor MySet [Prim (Poly "t")]]) (Constructor MyList [Constructor MySet [Prim Int]])
   == Just (Constructor MyList [Constructor MySet [Prim Int]])
-}
unify :: Type -> Type -> (Maybe Type, [Subst])
unify (Prim Int)  (Prim Int)      = (Just (Prim Int), [])
unify (Prim Bool) (Prim Bool)     = (Just (Prim Bool), [])

unify t@(Prim (Poly _))      (Prim (Poly tname))  = (Just t, substs) where substs = [Subst tname t]
unify t@(Prim _)          t'@(Prim (Poly tname))  = (Just (applySubsts substs t'), substs) where substs = [Subst tname t]
unify t@(Constructor _ _) t'@(Prim (Poly tname))  = (Just (applySubsts substs t'), substs) where substs = [Subst tname t]
unify tt@(Prim (Poly _))     tt'                  = unify tt' tt

unify (List t) (List t') = (Just (applySubsts substs t'), substs) where substs = snd $ unify t t'

unify f@(Func _ _) f'@(Func _ _) = if f == f' then (Just f, []) else (Nothing, [])

unify (Constructor tconstr types) (Constructor tconstr' types') =
  if tconstr == tconstr' && all (== length types) [length types', length unified]
      then (Just (Constructor tconstr (map (applySubsts substs) types)), substs)
      else (Nothing, [])
  where unified = [(t,s) | (Just t, s) <- zipWith unify types types']
        substs = concatMap snd [(t,s) | (Just t, s) <- zipWith unify types types']

unify _ _                 = (Nothing, [])


applySubsts :: [Subst] -> Type -> Type
applySubsts substs tt@(Prim (Poly tname)) =
  case newType of
    Just (Subst _ nt) -> nt
    Nothing           -> tt
  where newType = find (\(Subst tname' _) -> tname == tname') substs

applySubsts substs (List t) = List (applySubsts substs t)
applySubsts substs (Constructor tconstr types) = Constructor tconstr (map (applySubsts substs) types)
applySubsts substs (Func dom ran) = Func (map (applySubsts substs) dom) (applySubsts substs ran)
applySubsts _ t = t


specifyFunction :: Type -> Function -> Maybe Function
specifyFunction ran f =
  case unifiedType of
    (Just t, substs)   -> Just Function { name = name f, domain = map (applySubsts substs) (domain f), range = t }
    (Nothing, _)  -> Nothing
  where unifiedType = unify ran (range f)

functionsByRange :: Type -> [Function]
functionsByRange ran = mapMaybe (specifyFunction ran) functions

freshvar :: [Term] -> Type -> Identifier
freshvar env _ = Identifier ("x" ++ show i) where
  i = length $ filter (\t -> case t of Var _ _ -> True; _ -> False) env

data Env = Env { variables :: [Term], usedFuncs :: Map.Map String Int }

generate :: Env -> Type -> [Term]
generate env (Prim t) = Hole t :
  [Application f args | f <- functionsByRange (Prim t), args <- allowedArgs f] where
    allowedArgs f = sequence [generate env { usedFuncs = Map.insertWith (+) (name f) 1 (usedFuncs env) } t' | t' <- domain f]

generate env (List (Prim t)) = HoleList t :
  [Application f args | f <- functionsByRange (List (Prim t)), args <- allowedArgs f] where
    allowedArgs f = sequence [generate env { usedFuncs = Map.insertWith (+) (name f) 1 (usedFuncs env) } t' | t' <- domain f]

generate env (List t) =
  [Application f args | f <- functionsByRange (List t), args <- allowedArgs f] where
    allowedArgs f = sequence [generate env { usedFuncs = Map.insertWith (+) (name f) 1 (usedFuncs env) } t' | t' <- domain f]

generate env (Func dom ran) = [Abstraction vars body | body <- bodies] where
  vars = map (freshvar (variables env)) dom
  bodies = generate Env { variables = variables env ++ zipWith Var vars dom, usedFuncs = usedFuncs env } ran

generate env termType@(Constructor constr types) =
  [Application f args | f <- allowedFuncs, args <- allowedArgs f] where
    allowedArgs f = sequence [generate Env { variables = variables env, usedFuncs = Map.insertWith (+) (name f) 1 (usedFuncs env) } t | t <- domain f]
    allowedFuncs = filter isAllowed (functionsByRange termType)
    isAllowed func = case Map.lookup (name func) (usedFuncs env) of (Just n) -> n < 2; Nothing -> True

countFunc :: String -> Term -> Int
countFunc n (Application func terms) = (if name func == n then 1 else 0) + sum (map (countFunc n) terms)
countFunc _ _ = 0


testGen :: Type -> Int -> IO ()
testGen t n = mapM_ print (take n generated)
  where generated = generate Env{ variables = [], usedFuncs = Map.empty } t

main :: IO ()
main = putStr ""
