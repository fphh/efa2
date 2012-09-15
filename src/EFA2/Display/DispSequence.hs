{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module EFA2.Display.DispSequence where

import EFA2.Display.Report
import EFA2.Signal.SequenceData
import EFA2.Signal.Data ((:>))
import EFA2.Signal.Base (Val)

import qualified EFA2.Display.DispSignal as Disp
import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Data as D
import qualified EFA2.Signal.Vector as SV

import qualified Data.Map as M

import Data.Tuple.HT (mapFst)


instance ToTable Record where
         toTable os (ti,(Record time sigs)) = [Table {tableTitle = "Record - " ++ ti ,
                                                  tableData = tableData t,
                                                  tableFormat = tableFormat t,
                                                  tableSubTitle = ""}]

                                           where t = tvcat $ (toTable os ("Time",time)) ++ concatMap (toTable os . mapFst show) (M.toList sigs)



instance
   (SV.Walker v, SV.Singleton v, SV.FromList v, SV.Storage v Val,
    Disp.SigDisp S.Signal (v :> D.Nil)) =>
   ToTable (PowerRecord v Val) where
         toTable os (ti, PowerRecord time sigs) = [Table {tableTitle = "PowerRecord - " ++ ti ,
                                                  tableData = tableData t,
                                                  tableFormat = tableFormat t,
                                                  tableSubTitle = ""}]

                                           where t = tvcat $ (toTable os ("Time",time)) ++ concatMap (toTable os . mapFst show) (M.toList sigs)

instance (ToTable a) => ToTable (SequData a) where
         toTable os (_ti, SequData rs) = concatMap (toTable os) (zip (map f [0..]) rs)
           where f :: Int -> String
                 f idx = "Section " ++ show idx


instance ToTable Sequ where
   toTable _os (ti, Sequ xs) = [Table {tableTitle = "Sequence: " ++ ti,
                                      tableData = td,
                                      tableFormat = autoFormat td,
                                      tableSubTitle = ""}]
                        where
                          td = TableData {tableBody = [map f xs],
                                          titleRow  = [[toDoc id "Section:"]++map (\x -> toDoc id ("Sec" ++ show x)) [0..(length xs -1)]],
                                          titleCols = [[toDoc id "Index"]],
                                          endCols  = []}

                          -- f :: Sec -> TableData
                          f (i1, i2) = toDoc id $ show i1 ++ " - " ++ show i2
