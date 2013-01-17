{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.List as L
import qualified Data.Foldable as F

import Data.GraphViz.Types
  (DotRepr, parseDotGraph, graphNodes, graphEdges)

import Data.GraphViz.Types.Generalised
  ( DotNode(DotNode), DotGraph, fromNode, toNode,
    DotStatement(GA), graphStatements, GlobalAttributes(GraphAttrs))

import Data.GraphViz.Attributes.Complete
  (Attribute(UnknownAttribute, Label), Label(StrLabel))

import qualified Data.Text.Lazy as T

import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc (noLoc)

import System.Environment (getArgs)

--import Debug.Trace


getNodes :: (DotRepr dg n) => dg n -> [(n, String)]
getNodes = map f . graphNodes
  where f (DotNode str as) = (str,) $
          case map g as of
               [] -> "Crossing"
               [Just ty] -> ty
               _ -> error "Multiple types defined for node"

        g (UnknownAttribute tyStr ty) =
          case (T.unpack tyStr) of
               "type" -> Just (T.unpack ty)
               _ -> Nothing
        g _ = Nothing

getEdges :: (DotRepr dg n) => dg n -> [(n, n)]
getEdges = map f . graphEdges
  where f n = (fromNode n, toNode n)

---------------------------------------------------

cons :: Pat -> Pat -> Pat
cons = flip PInfixApp (UnQual $ sym ":~")

(.~) :: String -> String -> QName
x .~ y = Qual (ModuleName x) (name y)

(.$) :: Exp -> Exp -> Exp
(.$) = flip infixApp (op $ sym "$")

nodeStr :: String
nodeStr = "Node"

secStr :: String
secStr = "Section"

qualIdxStr :: String
qualIdxStr = "Idx"


qualTdStr :: String
qualTdStr = "TD"

qualStreamStr :: String
qualStreamStr = "Stream"

enumF :: Exp
enumF = Var $ "Stream" .~ "enumFrom"

enumFromFunc :: String -> Exp
enumFromFunc cstr = enumF .$ (appFun (Con $ qualIdxStr .~ cstr) [intE 0])

defPat :: [Name] -> Pat
defPat ns = L.foldr1 cons (ns' ++ [wildcard])
  where ns' = map pvar ns

definitions :: String -> [Name] -> Decl
definitions str ns = PatBind noLoc pat Nothing rhs bnds
  where pat = defPat ns
        rhs = UnGuardedRhs (enumFromFunc str)
        bnds = BDecls []

typeDecl :: String -> [Name] -> Decl
typeDecl str ns = TypeSig noLoc ns ty
  where ty = TyCon (qualIdxStr .~ str)


makeNodeDecls :: DotGraph String -> [Decl]
makeNodeDecls g = [typeDecl nodeStr ns, definitions nodeStr ns]
  where ns = map (Ident . fst) (getNodes g)


makeSections :: Int -> [Decl]
makeSections n = [typeDecl secStr secs, definitions secStr secs]
  where secs = map (Ident . ("sec" ++) . show) [0 .. n-1]


qualImport :: [String] -> String -> ImportDecl 
qualImport path qstr = ImportDecl {
  importLoc = noLoc,
  importModule = ModuleName (L.intercalate "." path),
  importQualified = True,
  importSrc = False,
  importPkg = Nothing,
  importAs = Just (ModuleName qstr),
  importSpecs = Nothing }

parenImport :: [String] -> [ImportSpec] -> ImportDecl 
parenImport path imps = ImportDecl {
  importLoc = noLoc,
  importModule = ModuleName (L.intercalate "." path),
  importQualified = False,
  importSrc = False,
  importPkg = Nothing,
  importAs = Nothing,
  importSpecs = Just (False, imps) }

idxImport :: ImportDecl
idxImport = qualImport ["EFA2", "Signal", "Index"] qualIdxStr

tdImport :: ImportDecl
tdImport = qualImport ["EFA2", "Topology", "TopologyData"] qualTdStr

streamImport :: ImportDecl
streamImport = qualImport ["EFA2", "Utils", "Stream"] qualStreamStr

streamOpImport :: ImportDecl
streamOpImport = parenImport ["EFA2", "Utils", "Stream"] specs
  where specs = [IThingWith (name "Stream") [ConName $ name "(:~)"]]

egImport :: ImportDecl
egImport = parenImport ["EFA2", "Topology", "EfaGraph"] specs
  where specs = [IVar (name "mkGraph")]

utImport :: ImportDecl
utImport = parenImport ["EFA2", "Example", "Utility"] specs
  where specs = [IVar (name "makeNodes"), IVar (name "makeEdges")]



extractLabel :: DotGraph String -> String
extractLabel g =
  case lbls of
       [] -> "defaultTopologyName"
       [Just str] -> T.unpack str
       _ -> error "Duplicate label attributes"
  where lbls = concatMap label $ F.toList $ graphStatements g
        label (GA (GraphAttrs as)) = map f as
        label _ = []
        f (Label (StrLabel str)) = Just str
        f _ = Nothing

topoTypeSig :: Name -> Decl
topoTypeSig ident = TypeSig noLoc [ident] ty
  where ty = TyCon (qualTdStr .~ "Topology")

makeTopology :: DotGraph String -> [Decl]
makeTopology g =
  topoTypeSig ident :
  PatBind noLoc lhs Nothing rhs (BDecls [numNodes, numEdges]) : []
  where ident = Ident $ extractLabel g
        lhs = pvar ident
        unqid = Var . UnQual . Ident
        rhs = UnGuardedRhs 
                (App (App (unqid "mkGraph")
                          (unqid "nodes"))
                          (Paren (App (unqid "makeEdges")
                                      (unqid "edges"))))
        nodes = getNodes g
        numNodes = PatBind 
                     noLoc 
                     (pvar $ name "nodes")
                     Nothing 
                     (UnGuardedRhs $ List $ map f nodes)
                     (BDecls [])
        f (n, t) = Tuple [var $ name n, Con (qualTdStr .~ t)]
        edges = getEdges g
        h (x, y) = Tuple [var $ name x, var $ name y]
        numEdges = PatBind
                     noLoc
                     (pvar $ name "edges")
                     Nothing
                     (UnGuardedRhs $ List $ map h edges)
                     (BDecls [])

mainFunc :: [Decl]
mainFunc = 
  TypeSig noLoc [ident] ty :
  PatBind noLoc lhs Nothing rhs (BDecls []) : []
  where ident = name "main"
        ty = TyCon (UnQual $ name "IO ()")
        lhs = pvar ident
        rhs = UnGuardedRhs $ var (name "undefined")

hModule :: [ImportDecl] -> [Decl] -> Module
hModule is ds = Module noLoc mName [] Nothing Nothing is ds
  where mName = ModuleName "Main"

parseArgs :: [String] -> (FilePath, Int)
parseArgs [file] = (file, 1)
parseArgs [file, "-sec", n] = (file, read n :: Int)
parseArgs args = error ("Malformed arguments: " ++ show args)


main :: IO ()
main = do
  args <- getArgs
  let (file, numSec) = parseArgs args
  str <- readFile file
  let g = parseDotGraph (T.pack str)
      imps = [idxImport, tdImport, streamImport, streamOpImport, egImport, utImport]
      decls = makeSections numSec ++ makeNodeDecls g ++ makeTopology g ++ mainFunc
      m = hModule imps decls
  putStrLn $ prettyPrint m
