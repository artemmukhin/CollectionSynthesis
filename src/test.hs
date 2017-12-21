module Main where
import Data.Map(Map, fromList, lookup)
import Data.List
import Data.Maybe(fromMaybe, catMaybes, mapMaybe, fromJust)

data PrimType
  = Int
  | Bool
  | Poly String
  deriving (Eq)

instance Show PrimType where
  show Int = "Int"
  show Bool = "Bool"
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

functionsMap :: Data.Map.Map String Function
functionsMap = Data.Map.fromList $ zip (map name functions) functions

getFunction :: String -> Function
getFunction funcname = fromMaybe (error "No such function") f
  where f = Data.Map.lookup funcname functionsMap

data Subst = Subst String Type deriving (Show)

{-
 Example:
 unify (Constructor MyList [Constructor MySet [Prim (Poly "t")]]) (Constructor MyList [Constructor MySet [Prim Int]])
   == Just (Constructor MyList [Constructor MySet [Prim Int]])
-}
unify :: Type -> Type -> (Maybe Type, [Subst])
unify (Prim Int)  (Prim Int)      = (Just (Prim Int), [])
unify (Prim Bool) (Prim Bool)     = (Just (Prim Bool), [])

unify (Prim (Poly t)) tt@(Prim (Poly t')) = (Just (applySubsts substs tt), substs) where substs = [Subst t' (Prim (Poly t))]
unify tt@(Prim _) tt'@(Prim (Poly t'))   = (Just (applySubsts substs tt'), substs) where substs = [Subst t' tt]
unify tt@(Constructor _ _) tt'@(Prim (Poly t')) = (Just (applySubsts substs tt'), substs) where substs = [Subst t' tt]
unify tt@(Prim (Poly _))   tt'   = unify tt' tt


unify (List t) (List t') = (Just (applySubsts substs t'), substs)
  where unifiedType = unify t t'
        substs = snd unifiedType

unify f@(Func _ _) f'@(Func _ _) = if f == f' then (Just f, []) else (Nothing, [])

unify (Constructor tconstr types) (Constructor tconstr' types') =
  if tconstr == tconstr' && all (== length types) [length types', length unifiedTypes]
      then (Just (Constructor tconstr (map (applySubsts substs) types)), substs)
      else (Nothing, [])
  where unifiedTypes =  [(t,s) | (Just t, s) <- zipWith unify types types']
        substs = concatMap snd unifiedTypes

unify _ _                 = (Nothing, [])


applySubsts :: [Subst] -> Type -> Type
applySubsts substs tt@(Prim (Poly name)) =
  case newType of
    Just (Subst _ nt) -> nt
    Nothing -> tt
  where newType = find (\(Subst name' _) -> name == name') substs

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


generate :: [Term] -> Type -> [Term]
generate env (Prim t) = Hole t :
  [Application f args | f <- functionsByRange (Prim t),
                        args <- sequence [generate env t' | t' <- domain f]]

generate env (List (Prim t)) = HoleList t :
  [Application f args | f <- functionsByRange (List (Prim t)),
                        args <- sequence [generate env t' | t' <- domain f]]

generate env (List t) =
  [Application f args | f <- functionsByRange (List t),
                        args <- sequence [generate env t' | t' <- domain f]]

generate env (Func dom ran) = [Abstraction vars body | body <- bodies] where
  vars = map (freshvar env) dom
  bodies = generate (env ++ zipWith Var vars dom) ran

generate env termType@(Constructor constr types) =
  [Application f args | f <- functionsByRange termType,
                        args <- sequence [generate env t | t <- domain f]]


main :: IO ()
main = putStr ""
