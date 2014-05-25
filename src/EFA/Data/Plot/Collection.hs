{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | PlotBase provide the basic functions to build Plots
module EFA.Data.Plot.Collection where

import qualified EFA.Data.Collection as Collection
--import qualified EFA.Data.ND.Cube.Map as CubeMap
--import qualified EFA.Data.ND as ND

--import qualified EFA.Signal.Sequence as Sequ
--import qualified EFA.Signal.Signal as S
--import qualified EFA.Signal.Data as D
import qualified EFA.Data.Vector as DV
--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Colour as Colour

import qualified EFA.Value.Type as ValueType
import qualified EFA.Value as Value

--import EFA.Signal.Record (Record(Record))
--import EFA.Signal.Signal (TC,
                        --  toSigList,
--                          getDisplayType)
--import EFA.Signal.Data (Data,
                        --(:>), Nil,
--                        NestedList)

import qualified EFA.Equation.Arithmetic as Arith
--import EFA.Equation.Arithmetic (
  --Sum,
--  Product, (~*), Constant)

--import qualified EFA.Graph.Topology.Node as Node

--import EFA.Report.Typ
--          (TDisp,
        --   DisplayType(Typ_T),
--           getDisplayUnit, getDisplayTypName)
--import EFA.Report.Base (UnitScale(UnitScale), getUnitScale)

--import qualified EFA.Report.Format as Format
--import EFA.Report.FormatValue (FormatValue, formatValue)

--import EFA.Utility.Show (showNode)

import qualified Graphics.Gnuplot.Advanced as Plot

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Plot as Plt
--import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
--import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
--import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
--import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Graph as Graph
--import qualified Graphics.Gnuplot.Value.Atom as Atom
--import qualified Graphics.Gnuplot.Value.Tuple as Tuple

--import qualified Graphics.Gnuplot.LineSpecification as LineSpec
--import qualified Graphics.Gnuplot.ColorSpecification as ColourSpec

import qualified Graphics.Gnuplot.Frame as Frame
--import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
--import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
--import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

--import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.Axis as Axis

--import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Foldable as Foldable
--import qualified Data.List.Key as Key
--import Data.Map (Map)
import Control.Functor.HT (void)
-- import Data.Foldable (foldMap)
--import Data.Monoid (mconcat)

--import EFA.Utility.Trace(mytrace)
import qualified EFA.Value.Type as Type
import Prelude hiding (sequence)

import EFA.Utility(Caller,(|>),
               --    merror,(|>),
                   ModuleName(..),FunctionName, genCaller)

import qualified  EFA.Data.Collection as Collection
import qualified  EFA.Data.Plot as DataPlot

modul :: ModuleName
modul = ModuleName "Data.Plot.Collection"

nc :: FunctionName -> Caller
nc = genCaller modul

instance 
  (Eq (Collection.OrdData a),
   Ord key,
   Collection.Unpack a, 
   DataPlot.ToPlotData key key a) => 
  DataPlot.ToPlotData id key (Collection.Collection key a) where
  toPlotData2 caller _ collection = 
    concatMap (\(key,obj) -> DataPlot.toPlotData2 (caller |> (nc "toPlotData"))  
                                                          (Just key) obj) $ Collection.toList collection