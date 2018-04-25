{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module SolutionLanguage where
import CollectionLanguage
import Data.Text (Text, pack)
import Data.Aeson hiding (Bool)


data Program =
  Program { recordDecls     :: [RecordDecl]
          , collectionDefs  :: [CollectionDef]
          , querieDefs      :: [QueryDef]
          } deriving Show

newtype Param = Param (String, PrimType) deriving (Eq, Show)
type Params = [Param]
type RecordDecl = Params

data CollectionDef = CollectionDef { cname :: Text, ctype :: Type, cinit :: Term } deriving (Show)

data QueryDef = QueryDef { qname :: Text, qparams :: Params, qbody :: Term } deriving (Show)

instance ToJSON Program where
  toJSON (Program rs cs qs) =
   object [ "recordDecls"      .= toJSON rs
          , "collectionDefs"   .= toJSON cs
          , "queriesDefs"      .= toJSON qs
          ]

instance ToJSON Param where
  toJSON (Param (name, t))    = object [ "name"  .= pack name, "type"  .= toJSON t ]

instance ToJSON PrimType where
  toJSON Int  = object [ "name" .= pack "Int" ]
  toJSON Bool = object [ "name" .= pack "Bool" ]
  toJSON (Poly s) = object [ "name" .= pack s ]
  toJSON (Record s _) = object [ "name" .= pack s ]

instance ToJSON CollectionDef where
  toJSON (CollectionDef name t expr) =
    object  [ "name"   .= name
            , "type"   .= toJSON t
            , "init"   .= toJSON expr
           ]

instance ToJSON Type where
  toJSON (Prim t) = toJSON t
  toJSON (Constructor constr types) =
    object [ "collection" .= toJSON constr
           , "typeParams" .= toJSON types
           ]
  toJSON _ = object []

instance ToJSON TypeConstructor where
  toJSON c = object [ "name" .= pack (show c) ]

instance ToJSON QueryDef where
  toJSON (QueryDef name params body) =
    object  [ "name"   .= name
            , "params" .= toJSON params
            , "body"   .= toJSON body
            ]

instance ToJSON Term where
  toJSON (Var ident t) = object [ "name" .= pack ident ]
  {-  object  [ "termType" .= pack "Var"
            , "name" .= pack ident
            , "type" .= toJSON t
            ]
  -}
  toJSON (Application func terms) =
    object  [ "termType" .= pack "Application"
            , "func" .= toJSON func
            , "args" .= toJSON terms
            ]
  toJSON (Abstraction ids term) =
    object  [ "termType" .= pack "Abstraction"
            , "params"   .= toJSON ids
            , "body"     .= toJSON term
            ]
  toJSON (Pair t1 t2) =
    object  [ "termType" .= pack "Pair"
            , "first"    .= toJSON t1
            , "second"   .= toJSON t2
            ]
  toJSON (Binary op t1 t2) =
    object  [ "termType"  .= pack "Binary"
            , "operator"  .= pack op
            , "left"      .= toJSON t1
            , "right"     .= toJSON t2
            ]

  toJSON _ = object []

instance ToJSON Function where
  toJSON (Function n d c) =
    object  [ "name"      .= pack n
            , "domain"    .= toJSON d
            , "codomain"  .= toJSON c
            ]

{-
data Person =
  Person { firstName  :: !Text
         , lastName   :: !Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving Show

instance ToJSON Person where
toJSON (Person firstName lastName age likesPizza) =
   object [ "firstName"  .= firstName
          , "lastName"   .= lastName
          , "age"        .= age
          , "likesPizza" .= likesPizza
            ]
-}
