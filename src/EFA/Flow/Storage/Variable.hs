{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Storage.Variable where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue (FormatScalarIndex, formatScalarIndex)

data Scalar part =
     Storage Idx.Storage
   | MaxEnergy Idx.MaxEnergy
   | Energy (Idx.StEnergy part)
   | X (Idx.StX part)
   | InSum (Idx.StInSum part)
   | OutSum (Idx.StOutSum part)
     deriving (Show, Eq, Ord)


ident :: Format output => Scalar t -> output
ident var =
   case var of
      Storage _idx -> Format.storage
      MaxEnergy _idx -> Format.maxEnergy
      Energy _idx -> Format.energy
      X _idx -> Format.xfactor
      InSum _idx -> Format.scalarSum
      OutSum _idx -> Format.scalarSum

instance Format.StorageIdx (Scalar part) where
   storageIdent (Idx.ForStorage var _node) = ident var


formatScalarValue ::
   (Format output, Format.Part part, Node.C node) =>
   Idx.ForStorage (Scalar part) node -> output
formatScalarValue (Idx.ForStorage var n) =
   case var of
      Storage idx -> formatScalarIndex idx n
      MaxEnergy idx -> formatScalarIndex idx n
      Energy idx -> formatScalarIndex idx n
      X idx -> formatScalarIndex idx n
      InSum idx -> formatScalarIndex idx n
      OutSum idx -> formatScalarIndex idx n

instance (Format.Part part) => FormatScalarIndex (Scalar part) where
   formatScalarIndex edge node = formatScalarValue (Idx.ForStorage edge node)
