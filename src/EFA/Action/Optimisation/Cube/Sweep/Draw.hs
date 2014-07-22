{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Optimisation.Cube.Sweep.Draw where    

import qualified EFA.Action.Optimisation.Cube.Sweep.Access as SweepAccess
import qualified EFA.Flow.Draw as Draw
import qualified EFA.Report.FormatValue as FormatValue
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Data.Vector as DV
import qualified EFA.Data.ND.Cube.Grid as CubeGrid
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.ND as ND
import qualified Data.Text.Lazy as LazyText
import qualified Data.GraphViz.Types.Canonical as Canonical

import EFA.Utility(Caller,
                   --merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "EFA.Action.Optimisation.Cube.Sweep.Draw"

nc :: FunctionName -> Caller
nc = genCaller modul

drawDemandSelection ::
  (Node.C node,
   FormatValue.FormatValue v,
   DV.Storage vec a,
   DV.LookupUnsafe vec (TopoQty.Section node v),
   DV.Length vec,
   ND.Dimensions dim) =>
  Caller ->
  String ->
  CubeGrid.ExtractInfo dim ->
  CubeMap.Cube inst dim label vec a (TopoQty.Section node v) ->
  [Canonical.DotGraph LazyText.Text]
drawDemandSelection caller title extractInfo sweep = map f $ SweepAccess.extractDemandData (caller |> nc "drawDemandSelection") sweep extractInfo
  where 
    f (dimIdx, sweepFlow) = Draw.title (title ++ " DimIdx: " ++ show dimIdx)  $ Draw.flowSection  Draw.optionsDefault sweepFlow

