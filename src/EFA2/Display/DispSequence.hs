module EFA2.Display.DispSequence (module EFA2.Display.DispSequence) where

import EFA2.Display.Report
import EFA2.Signal.SequenceData
import EFA2.Display.DispSignal
import qualified Data.Map as M 

instance ToTable PowerRecord where
         toTable os (ti,(PowerRecord time sigs)) = Table {tableTitle = "PowerRecord - " ++ ti , 
                                                  tableData = tableData t,
                                                  tableFormat = tableFormat t,            
                                                  tableSubTitle = ""}              
                                                    
                                           where t = tvcat $ [toTable os ("Time",time)] ++ (map (toTable os . f) (M.toList sigs))
                                                 f (x,y) = (show x,y)
  