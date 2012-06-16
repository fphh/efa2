module EFA2.Display.DispSequence (module EFA2.Display.DispSequence) where

import EFA2.Display.Report
import EFA2.Signal.SequenceData

instance ToTable PowerRecord where
         toTable (PowerRecord time sigs) = map toTable sigs
  