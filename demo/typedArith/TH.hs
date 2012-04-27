{-# LANGUAGE TemplateHaskell, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, NoMonomorphismRestriction #-}

module EFA2.Signal.TH where

import Control.Monad

import Language.Haskell.TH
import Language.Haskell.TH.Lib

 
class Type (typ :: * -> *) x where
  toType :: x -> typ x
  fromType :: typ x -> x



printQ expr = runQ expr >>= putStrLn.pprint

samples :: [String]
samples = ["E","N","M","P","T","X","DE","DN","DM","DP","DT","DX"]

sampleNames :: [Name]
sampleNames = map mkName samples

showN = mkName "Show"
eqN = mkName "Eq"
numN = mkName "Num"
contN = mkName "cont"
valN = mkName "Val"


standaloneDerive :: Name -> Q [Dec]
standaloneDerive s = return (concatMap f [showN, eqN])
  where f x = [InstanceD [ClassP x [AppT (VarT contN) (ConT valN)]] (AppT (ConT x) (AppT (ConT s) (VarT contN))) []]

mkDeriving :: [Name] -> Q [Dec]
mkDeriving names = liftM concat $ mapM standaloneDerive names

sampleNewtype :: String -> Q [Dec]
sampleNewtype str = do
  let ty = mkName str
      getter = mkName ("un" ++ str)
  var <- newName "cont"
  return $ [NewtypeD [] ty [PlainTV var] (RecC ty [(getter, NotStrict, AppT (VarT var) (ConT valN))]) []]

mkNewtypes :: [String] -> Q [Dec]
mkNewtypes strs = liftM concat $ mapM sampleNewtype strs


sampleInstance :: Name -> Q [Dec]
sampleInstance name = do
  TyConI (NewtypeD _ name tyvars (RecC constr _) _) <- reify name
  ClassI (ClassD _ cname ctyvars _ cdecs) _ <- reify (mkName "Sample")
  var <- newName "x"
  let [SigD fname _, SigD tname _] = cdecs
      header = AppT (AppT (ConT cname) (VarT (mkName "cont"))) (ConT name)
      vare = VarE var
      varp = VarP var
      fromf = FunD fname [Clause [ConP constr [varp]] (NormalB vare) []]
      tof = FunD tname [Clause [varp] (NormalB (AppE (ConE constr) vare)) []]
  return $ [InstanceD [] header [fromf, tof]]

mkInstances :: [Name] -> Q [Dec]
mkInstances names = liftM concat $ mapM sampleInstance names