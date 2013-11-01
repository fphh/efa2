{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Storage.Variable where

import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue (FormatScalarIndex, formatScalarIndex)

data Scalar part =
     Content StorageIdx.Content
   | MaxEnergy StorageIdx.MaxEnergy
   | Energy (StorageIdx.Energy part)
   | X (StorageIdx.X part)
   | InSum (StorageIdx.InSum part)
   | OutSum (StorageIdx.OutSum part)
     deriving (Show, Eq, Ord)


ident :: Format output => Scalar t -> output
ident var =
   case var of
      Content _idx -> Format.storage
      MaxEnergy _idx -> Format.maxEnergy
      Energy _idx -> Format.energy
      X _idx -> Format.xfactor
      InSum _idx -> Format.scalarSum
      OutSum _idx -> Format.scalarSum

instance Format.StorageIdx (Scalar part) where
   storageIdent (Idx.ForStorage var _node) = ident var


formatScalarValue ::
   (Format output, Format.Part part) =>
   Scalar part -> (output, output)
formatScalarValue var =
   case var of
      Content idx -> formatScalarIndex idx
      MaxEnergy idx -> formatScalarIndex idx
      Energy idx -> formatScalarIndex idx
      X idx -> formatScalarIndex idx
      InSum idx -> formatScalarIndex idx
      OutSum idx -> formatScalarIndex idx

instance (Format.Part part) => FormatScalarIndex (Scalar part) where
   formatScalarIndex var = formatScalarValue var
