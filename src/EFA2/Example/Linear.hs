{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}


module EFA2.Example.Linear where

import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import EFA2.Graph.Graph
import EFA2.Graph.GraphData
import EFA2.Signal.Sequence
import EFA2.Signal.SignalGeneration

rec :: Record
rec = Record time  sigMap
   where
     time = list2Time [0,1]
     sigMap = M.fromList [(SigIdent "0",list2Power [3, 3]), 
                          (SigIdent "1",list2Power [2.2, 2.2]), 
                          (SigIdent "2",list2Power [1.8, 1.8]),
                          (SigIdent "3",list2Power [1, 1]),
                          (SigIdent "4",list2Power [0.4, 0.4]),
                          (SigIdent "5",list2Power [0.2, 0.2])]

-- Map Signals via SignalIdent to 
mapping :: Mapping
mapping =  Mapping $ MPointData $ M.fromList [(mkIdx 0 1,(SigIdent "0", DontFlip)),
                                            (mkIdx 1 0,(SigIdent "1", DontFlip)),
                                            (mkIdx 1 2,(SigIdent "2", DontFlip)),
                                            (mkIdx 2 1,(SigIdent "3", DontFlip)),
                                            (mkIdx 2 3,(SigIdent "4", DontFlip)),
                                            (mkIdx 3 2,(SigIdent "5", DontFlip))]


linear :: (Gr NLabel ELabel, Record, Mapping)
linear = (g, rec, mapping)
  where g = mkGraph (makeNodes no) (makeEdges no)
        no = [0..3]
