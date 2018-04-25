module JSONTest where
import CollectionLanguage
import Solution2JSON
import Data.Text (Text, pack)
import Data.Aeson hiding (Bool)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO as IO

saveFile :: IO ()
saveFile = IO.writeFile "prog.json" (encodeToLazyText (toJSON prog))

prog :: Program
prog = Program { recordDecls = [edge], collectionDefs = [graph], querieDefs = qs }

edge :: RecordDecl
edge = [ Param ("src", Int), Param ("dst", Int) ]

edgeType :: PrimType
edgeType = Record "Edge" [("src", Int), ("dst", Int)]

graph :: CollectionDef
graph = CollectionDef {
  cname = pack "table",
  ctype = Constructor MyMap [Prim Int, Constructor MySet [Prim edgeType]],
  cinit = mapInit }

graphTerm :: Term
graphTerm = Var "graph" (Constructor MyMap [Prim Int, Prim edgeType])

-- Map.build (e -> (e.src, filter (e' -> e'.src = e.src) graph)) graph
mapInit :: Term
mapInit = Application (functions !! 5) [mapFunc, graphTerm]

eTerm :: Term
eTerm = Var "e" (Prim edgeType)
e'Term :: Term
e'Term = Var "e'" (Prim edgeType)

srcTerm :: Term
srcTerm = Var "src" (Prim Int)


mapFunc :: Term
mapFunc = Abstraction ["e"] (Pair (Binary "." eTerm srcTerm) filterTerm)

filterTerm :: Term
filterTerm = Application (functions !! 4) [filterFunc, graphTerm]

filterFunc :: Term
filterFunc = Abstraction ["e'"] (Binary "==" (Binary "." e'Term srcTerm) (Binary "." eTerm srcTerm))


qs :: [QueryDef]
qs = []
