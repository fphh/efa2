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


class FormatScalarIndex t => Index t where
   type Part t :: *
   index :: t -> Scalar (Part t)

instance Index StorageIdx.Content where
   type Part StorageIdx.Content = Idx.Section
   index = Content

instance Index StorageIdx.MaxEnergy where
   type Part StorageIdx.MaxEnergy = Idx.Section
   index = MaxEnergy

instance (Format.Part part) => Index (StorageIdx.Energy part) where
   type Part (StorageIdx.Energy part) = part
   index = Energy

instance (Format.Part part) => Index (StorageIdx.X part) where
   type Part (StorageIdx.X part) = part
   index = X

instance (Format.Part part) => Index (StorageIdx.InSum part) where
   type Part (StorageIdx.InSum part) = part
   index = InSum

instance (Format.Part part) => Index (StorageIdx.OutSum part) where
   type Part (StorageIdx.OutSum part) = part
   index = OutSum


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
   carryIdent (Idx.ForStorage var _node) = ident var


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