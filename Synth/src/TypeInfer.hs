{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TypeInfer where
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy
import Data.List (nub)

import CoreLanguage

data Scheme = Forall [TVar] Type

newtype Unique = Unique { count :: Int }

newtype TypeEnv = TypeEnv (Map.Map Variable Scheme)
instance Monoid TypeEnv where
  mempty = TypeEnv Map.empty
  mappend (TypeEnv env) (TypeEnv env') = TypeEnv (env `Map.union` env')


extend :: TypeEnv -> (Variable, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String

type Infer = ExceptT TypeError (State Unique)

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique { count = 0 }

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> Variable -> Maybe Scheme
typeof (TypeEnv env) name = Map.lookup name env

type Subst = Map.Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ Unknown    = Unknown
  apply _ Int        = Int
  apply _ Bool       = Bool
  apply s (List a)   = List (apply s a)
  apply s t@(TVar a) = Map.findWithDefault t a s

  ftv Unknown        = Set.empty
  ftv Int            = Set.empty
  ftv Bool           = Set.empty
  ftv (List a)       = ftv a
  ftv (TVar a)       = Set.singleton a

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env


normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord :: [(TVar, TVar)]
    ord = zip (nub $ fv body) (fmap TV letters)

    fv Unknown  = []
    fv Int      = []
    fv Bool     = []
    fv (TVar a) = [a]
    fv (List a) = fv a

    normtype Unknown  = Unknown
    normtype Int      = Int
    normtype Bool     = Bool
    normtype (List a) = List (normtype a)

    normtype (TVar a)   =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']
