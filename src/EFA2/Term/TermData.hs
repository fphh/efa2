

module EFA2.Term.TermData where

import EFA2.Graph.GraphData
import EFA2.Signal.TH


data Abs
data Diff

data InTerm a = PIdx PowerIdx
              | EIdx EtaIdx
              | ScaleIdx XIdx
              | InConst Val
              | InMinus (InTerm a)
              | InRecip (InTerm a)
              | InAdd (InTerm a) (InTerm a)
              | InMult (InTerm a) (InTerm a)
              | InEqual (InTerm a) (InTerm a) deriving (Eq, Ord, Show)
