{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | PlotBase provide the basic functions to build Plots
module EFA.Data.Plot {-(
   run,
   signal,
   signalFrameAttr,
   heatmap, xyzrange3d, cbrange, xyzlabel, xyzlabelnode, depthorder,
   paletteGH, paletteGray, paletteHSV, missing, contour,
   Signal,
   Value,
   Labeled, label,
   xy,
   xyBasic,
   xyStyle,
   xyFrameAttr,
   XY,
   surface,
   Surface,
   record,
   recordFrameAttr,
   recordList,
   sequence,
   optKeyOutside,
   stack,
   stackFrameAttr,
   stacks,
   stacksFrameAttr,
   getData,
   genAxLabel
   ) -}
       where

--import qualified EFA.Data.ND.Cube.Map as CubeMap
--import qualified EFA.Data.ND as ND

--import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Data as D
import qualified EFA.Data.Vector as DV
--import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Colour as Colour

import qualified EFA.Value.Type as ValueType
import qualified EFA.Value as Value

--import EFA.Signal.Record (Record(Record))
import EFA.Signal.Signal (TC,
                        --  toSigList,
                          getDisplayType)
import EFA.Signal.Data (Data,
                        --(:>), Nil,
                        NestedList)

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic (
  --Sum,
  Product, (~*), Constant)

--import qualified EFA.Graph.Topology.Node as Node

import EFA.Report.Typ
          (TDisp,
        --   DisplayType(Typ_T),
           getDisplayUnit, getDisplayTypName)
import EFA.Report.Base (UnitScale(UnitScale), getUnitScale)

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
import qualified Graphics.Gnuplot.Value.Atom as Atom
--import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.ColorSpecification as ColourSpec

import qualified Graphics.Gnuplot.Frame as Frame
--import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
--import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
--import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

--import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.Axis as Axis

--import qualified Data.Map as Map
import qualified Data.List as List
--import qualified Data.Foldable as Fold
--import qualified Data.List.Key as Key
--import Data.Map (Map)
import Control.Functor.HT (void)
--import Data.Foldable (foldMap)
--import Data.Monoid (mconcat)

--import EFA.Utility.Trace(mytrace)
import qualified EFA.Value.Type as Type
import Prelude hiding (sequence)

import EFA.Utility(Caller,
               --    merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
--import Data.List as List

modul :: ModuleName
modul = ModuleName "Data.Plot"

nc :: FunctionName -> Caller
nc = genCaller modul

{-
data TicsInfo label = TicsInfo (Maybe label) [Double] ValueType.Dynamic

getTicsInfo ::
  (Show label, Ord a,
   DV.Storage vec a,
   DV.FromList vec,
   ValueType.GetDynamicType a,
   DV.Singleton vec,
   Value.ToDouble a) =>
  Strict.Axis typ label vec a -> TicsInfo label
getTicsInfo axis = TicsInfo
  (Just $ Axis.getLabel axis)
  (map Value.toDouble $ DV.toList $ Axis.getVector axis)
  (Axis.getType axis)

data RangeInfo = RangeInfo (Value.Range Double) (ValueType.Dynamic)

getRangeInfo ::
  (Value.ToDouble a,
   Type.GetDynamicType a,
   Ord a,
   DV.Storage vec a,
   DV.Singleton vec) =>
  vec a -> RangeInfo
getRangeInfo dataVec = RangeInfo (fmap Value.toDouble range)
                      (ValueType.getDynamicType range)
     where range = (\(x,y) -> Value.Range x y) $ DV.minmax dataVec

data AxisInfo label = Tics (TicsInfo label) | Range RangeInfo
-}

data AxisInfo label a = AxisInfo [Maybe label] (Value.Range a) (Tics2  a) [ValueType.Dynamic] deriving Show
data Tics2 a = NoTics2 | Tics2 [a]  deriving Show

fromAxis ::
  (Constant a, DV.Storage vec a, Axis.GetInfo axis vec a,
   DV.FromList vec) =>
  (axis:: * -> * -> (* -> *) -> * -> *) typ label vec a -> AxisInfo label a
fromAxis axis =
  AxisInfo [Just $ Axis.getLabel axis]
            (fmap (Type.toDisplayUnit' typ) $ Axis.getRange axis)
            (Tics2 $ map (Type.toDisplayUnit' typ) $ DV.toList $ Axis.getVector axis)
            [typ]
  where
--  ax = (DV.toList $ Axis.getVector axis)
  typ = Axis.getType axis

fromRange ::
  (Ord a, Constant a,
   DV.Storage vec a, DV.Singleton vec,
   Type.GetDynamicType a) =>
  vec a -> AxisInfo label a
fromRange dataVec =
  AxisInfo
  [Nothing]
  (fmap (Type.toDisplayUnit' typ) range)
  NoTics2
  [typ]
     where range = (\(x,y) -> Value.Range x y) $ DV.minmax dataVec
           typ = ValueType.getDynamicType range


-- TODO: Tics better with set datatype ?
combineTics :: Ord a => Tics2 a -> Tics2  a -> Tics2 a
combineTics NoTics2 NoTics2 = NoTics2
combineTics (Tics2 xs) NoTics2 = Tics2 xs
combineTics NoTics2 (Tics2 xs) = Tics2 xs
combineTics (Tics2 xs) (Tics2 xs1) = Tics2 $ List.sort $ xs ++ xs1


combineList :: Ord a => [AxisInfo label a] -> AxisInfo label a
combineList (x:xs) = foldl combine x xs

combine :: Ord a =>
  AxisInfo label a ->
  AxisInfo label a ->
  AxisInfo label a
combine (AxisInfo label range tic typ) (AxisInfo label1 range1 tic1 typ1) =
          (AxisInfo (label++label1)
           (Value.combineRange range range1)
           (combineTics tic tic1)
           (typ++typ1))
{-
combineWithId ::
  (AxisInfo label, PlotInfo id a) ->
  (AxisInfo label, PlotInfo id a) ->
  AxisInfo label
combine
  (AxisInfo label range tic typ, PlotInfo ident)
  (AxisInfo label1 range1 tic1 typ1, PlotInfo ident1) =
          (AxisInfo (label++label1)
           (Value.combineRange range range1)
           (combineTics tic tic1)
           (typ++typ1))
-}


makeAxisLabel :: Show label => AxisInfo label a -> String
makeAxisLabel (AxisInfo labels _ _ types) =
  if all (== head labelList) labelList
  then head labelList
  else  List.intercalate "," labelList
  where f (Just l) t = (show l) ++ " [" ++ (Type.showUnit $ Type.getDisplayUnit t) ++ "] "
        f (Nothing) t = "-" ++ " [" ++ (Type.showUnit $ Type.getDisplayUnit t) ++ "] "
        labelList = zipWith f labels types

makeAxisLabelWithIds  :: Show id => [Maybe id] -> AxisInfo label a -> String
makeAxisLabelWithIds xs (AxisInfo _ _ _ types) =
  if all (== head labelList) labelList
  then head labelList
  else  List.intercalate "," labelList
    where f (Just l) t = (show l) ++ " [" ++ (Type.showUnit $ Type.getDisplayUnit t) ++ "] "
          f (Nothing) t = "-" ++ " [" ++ (Type.showUnit $ Type.getDisplayUnit t) ++ "] "
          labelList = zipWith f xs types



--Type.showUnit $ Type.getDisplayUnit

-- | Generic IO Commands ---------------------------------------------------------------
run ::
   (Terminal.C term, Graph.C graph) =>
   term -> Opts.T graph -> Plt.T graph -> IO ()
run terminal frameAttr plt =
   void $ Plot.plotSync terminal $ Frame.cons frameAttr plt



{-
-- | Example how to generate frame attributes

frameAttr ::
   (AxisLabel tc, Graph.C graph) =>
   String -> tc -> Opts.T graph
framAttr ti x =
   Opts.title ti $
   Opts.xLabel "Signal Index []" $
   Opts.yLabel (genAxLabel x) $
   Opts.grid True $
   Opts.deflt
-}

-- TODO check if AxisLabel is reusable


-- | Class to generate Axis Labels

class Atom.C (Value tc) => AxisLabel tc where
   type Value tc :: *
   genAxLabel :: tc -> String

instance (TDisp t, Atom.C (D.Value c)) => AxisLabel (TC s t c) where
   type Value (TC s t c) = D.Value c
   genAxLabel x =
      let dispType = getDisplayType x
      in  getDisplayTypName dispType ++
             " [" ++ (show $ getDisplayUnit dispType) ++ "]"

instance (AxisLabel tc) => AxisLabel [tc] where
   type Value [tc] = Value tc
   genAxLabel x = genAxLabel $ head x


-- | Get Signal Plot Data (Unit Conversion)  ---------------------------------------------------------------

getData ::
   (TDisp typ, D.FromList c, D.Map c, D.Storage c a, Constant a) =>
   TC s typ (Data c a) -> NestedList c a
getData x = S.toList $ S.map (~* Arith.fromRational s) x
   where (UnitScale s) = getUnitScale $ getDisplayUnit $ getDisplayType x


-- | Function to simplify linecolor setting

lineColour :: Colour.Name -> LineSpec.T -> LineSpec.T
lineColour = LineSpec.lineColor . ColourSpec.name . Colour.unpackName


data PlotInfo id a = PlotInfo (Maybe id) (Maybe a)