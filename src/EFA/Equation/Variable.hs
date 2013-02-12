module EFA.Equation.Variable where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Symbolic.OperatorTree as OT
import qualified EFA.Symbolic.SumProduct as SP
import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)


data Index a = Energy (Idx.Energy a)
             | DEnergy (Idx.DEnergy a)
             | MaxEnergy (Idx.MaxEnergy a)
             | DMaxEnergy (Idx.DMaxEnergy a)
             | Power (Idx.Power a)
             | DPower (Idx.DPower a)
             | Eta (Idx.Eta a)
             | DEta (Idx.DEta a)
             | DTime (Idx.DTime a)
             | X (Idx.X a)
             | DX (Idx.DX a)
             | Y (Idx.Y a)
             | DY (Idx.DY a)
             | Var (Idx.Var a)
             | Store (Idx.Storage a)
               deriving (Show, Eq, Ord)


class MkIdxC t where
   mkIdx :: t a -> Index a
   --mkIdx :: a n -> Index n


instance MkIdxC Idx.Energy where mkIdx = Energy
instance MkIdxC Idx.DEnergy where mkIdx = DEnergy
instance MkIdxC Idx.MaxEnergy where mkIdx = MaxEnergy
instance MkIdxC Idx.DMaxEnergy where mkIdx = DMaxEnergy
instance MkIdxC Idx.Power where mkIdx = Power
instance MkIdxC Idx.DPower where mkIdx = DPower
instance MkIdxC Idx.Eta where mkIdx = Eta
instance MkIdxC Idx.DEta where mkIdx = DEta
instance MkIdxC Idx.DTime where mkIdx = DTime
instance MkIdxC Idx.X where mkIdx = X
instance MkIdxC Idx.DX where mkIdx = DX
instance MkIdxC Idx.Y where mkIdx = Y
instance MkIdxC Idx.DY where mkIdx = DY
instance MkIdxC Idx.Var where mkIdx = Var
instance MkIdxC Idx.Storage where mkIdx = Store


class MkVarC term where
      mkVarCore :: a -> term a

instance MkVarC OT.Term where
         mkVarCore = OT.Atom

instance MkVarC SP.Term where
         mkVarCore = SP.Atom

mkVar :: (MkIdxC t, MkVarC term) => t a -> term (Index a)
mkVar = mkVarCore . mkIdx

instance (Show nty) => FormatValue (Index nty) where
   formatValue idx =
      let absolute e r x y = Format.edgeIdent e
             -- Format.subscript (Format.edgeIdent e) (Format.edgeIndex r x y)
          delta e r x y = Format.delta $ Format.edgeIdent e
             -- Format.subscript (Format.delta $ Format.edgeIdent e)
             --                 (Format.edgeIndex r x y)
      in  case idx of
             Energy (Idx.Energy r x y) -> absolute Format.Energy r x y
             DEnergy (Idx.DEnergy r x y) -> delta Format.Energy r x y

             MaxEnergy (Idx.MaxEnergy r x y) ->
               absolute Format.MaxEnergy r x y
             DMaxEnergy (Idx.DMaxEnergy r x y) -> delta Format.MaxEnergy r x y

             Power (Idx.Power r x y) -> absolute Format.Power r x y
             DPower (Idx.DPower r x y) -> delta Format.Power r x y

             Eta (Idx.Eta r x y) -> absolute Format.Eta r x y
             DEta (Idx.DEta r x y) -> delta Format.Eta r x y

             X (Idx.X r x y) -> absolute Format.X r x y
             DX (Idx.DX r x y) -> delta Format.X r x y

             Y (Idx.Y r x y) -> absolute Format.Y r x y
             DY (Idx.DY r x y) -> delta Format.Y r x y

             DTime (Idx.DTime r s) ->
                Format.subscript (Format.delta Format.time) $
                   Format.record r `Format.connect` Format.section s

             Var (Idx.Var r u x) ->
                Format.subscript Format.var $
                   Format.record r `Format.connect`
                   Format.use u `Format.connect` Format.sectionNode x

             Store (Idx.Storage r x) ->
                Format.subscript Format.storage $
                   Format.record r `Format.connect` Format.sectionNode x
