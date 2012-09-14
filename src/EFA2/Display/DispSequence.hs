{-# LANGUAGE TypeSynonymInstances #-}
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
                                                    
                                           where t = tvcat $ (toTable os ("Time",time)) ++ concat (map (toTable os . f) (M.toList sigs))
                                                 f (x,y) = (show x,y)



instance
   (SV.Walker v Val Val, SV.Singleton v Val, SV.FromList v Val,
    Disp.SigDisp S.Signal (v :> D.Nil)) =>
   ToTable (PowerRecord v Val) where
         toTable os (ti, PowerRecord time sigs) = [Table {tableTitle = "PowerRecord - " ++ ti ,
                                                  tableData = tableData t,
                                                  tableFormat = tableFormat t,
                                                  tableSubTitle = ""}]

                                           where t = tvcat $ (toTable os ("Time",time)) ++ concatMap (toTable os . mapFst show) (M.toList sigs)

instance ToTable SequPwrRecord where
         toTable os (ti,(SequData rs)) = concat $ map (toTable os) (zipWith f [0..] rs)
           where f idx r = ("Section " ++ show idx,r)
                                                                           

instance ToTable Sequ where
         toTable os (ti,xs) = [Table {tableTitle = "Sequence: " ++ ti,
                                      tableData = td,
                                      tableFormat = autoFormat td,
                                      tableSubTitle = ""}]
                        where
                          td = TableData {tableBody = [map f xs],
                                          titleRow  = [[toDoc id "Section:"]++map (\x -> toDoc id ("Sec" ++ show x)) [0..(length xs -1)]],
                                          titleCols = [[toDoc id "Index"]],
                                          endCols  = []}
                          
                          -- f :: (Int,Int) -> TableData
                          f (i1, i2) = toDoc id $ (show i1) ++ " - " ++ (show i2) 