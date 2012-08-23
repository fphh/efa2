{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module EFA2.Display.DispSequence (module EFA2.Display.DispSequence) where

import EFA2.Display.Report
import EFA2.Signal.SequenceData
import EFA2.Display.DispSignal
import EFA2.Signal.Base
import EFA2.Signal.Signal

import qualified Data.Map as M 

instance ToTable Record where
         toTable os (ti,(Record time sigs)) = [Table {tableTitle = "Record - " ++ ti , 
                                                  tableData = tableData t,
                                                  tableFormat = tableFormat t,            
                                                  tableSubTitle = ""}]              
                                                    
                                           where t = tvcat $ (toTable os ("Time",time)) ++ concat (map (toTable os . f) (M.toList sigs))
                                                 f (x,y) = (show x,y)


instance ToTable PowerRecord where
         toTable os (ti,(PowerRecord time sigs)) = [Table {tableTitle = "PowerRecord - " ++ ti , 
                                                  tableData = tableData t,
                                                  tableFormat = tableFormat t,            
                                                  tableSubTitle = ""}]              
                                                    
                                           where t = tvcat $ (toTable os ("Time",time)) ++ concat (map (toTable os . f) (M.toList sigs))
                                                 f (x,y) = (show x,y)
  
instance ToTable SecPowerRecord where
         toTable os (ti,(SecPowerRecord time sigs)) = [Table {tableTitle = "SectionPowerRecord - " ++ ti , 
                                                  tableData = tableData t,
                                                  tableFormat = tableFormat t,            
                                                  tableSubTitle = ""}]              
                                                    
                                           where t = tvcat $ (toTable os ("Time",time)) ++ concat (map (toTable os . f) (M.toList sigs))
                                                 f (x,y) = (show x,y)

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