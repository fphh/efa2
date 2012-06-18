{-# LANGUAGE TypeSynonymInstances #-}

module EFA2.Display.DispSequence (module EFA2.Display.DispSequence) where

import EFA2.Display.Report
import EFA2.Signal.SequenceData
import EFA2.Display.DispSignal
import qualified Data.Map as M 

instance ToTable Record where
         toTable os (ti,(Record time sigs)) = Table {tableTitle = "Record - " ++ ti , 
                                                  tableData = tableData t,
                                                  tableFormat = tableFormat t,            
                                                  tableSubTitle = ""}              
                                                    
                                           where t = tvcat $ [toTable os ("Time",time)] ++ (map (toTable os . f) (M.toList sigs))
                                                 f (x,y) = (show x,y)


instance ToTable PowerRecord where
         toTable os (ti,(PowerRecord time sigs)) = Table {tableTitle = "PowerRecord - " ++ ti , 
                                                  tableData = tableData t,
                                                  tableFormat = tableFormat t,            
                                                  tableSubTitle = ""}              
                                                    
                                           where t = tvcat $ [toTable os ("Time",time)] ++ (map (toTable os . f) (M.toList sigs))
                                                 f (x,y) = (show x,y)
  
instance ToTable SecPowerRecord where
         toTable os (ti,(SecPowerRecord time sigs)) = Table {tableTitle = "SectionPowerRecord - " ++ ti , 
                                                  tableData = tableData t,
                                                  tableFormat = tableFormat t,            
                                                  tableSubTitle = ""}              
                                                    
                                           where t = tvcat $ [toTable os ("Time",time)] ++ (map (toTable os . f) (M.toList sigs))
                                                 f (x,y) = (show x,y)

instance ToTable SequPwrRecord where
         toTable os (ti,(SequData rs)) = thcat $ map (toTable os) (zipWith f [0..] rs)
           where f idx r = ("Section " ++ show idx,r)
                                                                           
                                                                           
                                              
