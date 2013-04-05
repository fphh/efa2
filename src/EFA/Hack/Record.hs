{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts#-}

module EFA.Hack.Record where

import qualified EFA.Graph.Topology.Index as Idx
-- import qualified EFA.Signal.Signal as S
-- import qualified EFA.Signal.Data as D
-- import qualified EFA.Signal.Vector as V
import EFA.Signal.Signal
          (--TC, 
           Signal 
           --FSignal, 
           --TSigL, 
           -- UTSignal, 
           --TSignal,
           --TSamp, 
           --PSamp, 
           --PSamp1L, 
           --PSamp2LL,
           --Scal
          )

import EFA.Signal.Typ (Typ, 
                       A, 
                       P, 
                       T, 
                       Tt 
                       --UT,
                       --F,
                       --D
                      )
-- import EFA.Signal.Data (--Data, 
--                        (:>), 
--                        Nil)
-- import EFA.Signal.Base (Sign, 
--                        BSum, 
--                        BProd)

-- import EFA.Report.Report (ToTable(toTable), Table(..), tvcat)
--import EFA.Report.Typ (TDisp, getDisplayTypName)
-- import EFA.Report.Base (DispStorage1)

-- import Text.Printf (PrintfArg)
-- import qualified Test.QuickCheck as QC
-- import System.Random (Random)

import qualified Data.Map as M
-- import qualified Data.Set as Set
-- import qualified Data.List.HT as HTL
--import qualified Data.List.Key as Key
--import qualified Data.List.Match as Match

--import Data.NonEmpty ((!:))
--import Data.Ratio (Ratio, (%))
--import Data.Foldable (foldMap)
--import Data.List (transpose)
--import Data.Tuple.HT (mapFst)
--import Control.Monad (liftM2)
import EFA.Utility (checkedLookup)

import EFA.Signal.Record

-- Plot Records with readible keys 
namePowers :: (Ord node, Show node,Show (v a)) =>  M.Map (Idx.PPos node) SigId -> PowerRecord node v a -> Record Signal (Typ A T Tt) (Typ A P Tt) SigId v a
namePowers powerNames (Record time pMap) = Record time newMap
  where -- replace old with new keys
    newMap = M.mapKeys f pMap
    f key = checkedLookup powerNames key

recName :: String -> Record s t1 t2 SigId v a -> Record s t1 t2 SigId v a
recName name (Record time sigs) = Record time (M.mapKeys (\ (SigId x) -> SigId (name ++ "_" ++ x) ) sigs)
