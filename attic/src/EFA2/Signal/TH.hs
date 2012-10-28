{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EFA2.Signal.TH where

import Control.Monad (liftM)

import Language.Haskell.TH

 

type Val = Double

class Sample cont dim where
      fromSample :: dim cont -> cont Val
      toSample :: cont Val -> dim cont


printQ :: Ppr a => Q a -> IO ()
printQ expr = runQ expr >>= putStrLn.pprint

samples :: [String]
samples = ["PSample", "NSample"]

sampleNames :: [Name]
sampleNames = map mkName samples

showN, eqN, numN, contN, valN :: Name
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
sampleInstance iname = do
  TyConI (NewtypeD _ name _tyvars (RecC constr _) _) <- reify iname
  ClassI (ClassD _ cname _ctyvars _ cdecs) _ <- reify (mkName "Sample")
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
