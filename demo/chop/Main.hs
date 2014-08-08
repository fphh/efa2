module Main where

import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Data.OD.Signal.Chop as Chop
import qualified EFA.Data.OD.Signal.Flow as SignalFlow
import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Value.Type as Type
import qualified Data.Map as Map

import EFA.Utility(Caller,
                   --merror,(|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Demo.Chop"

nc :: FunctionName -> Caller
nc = genCaller modul

data Node =
     Coal
   | Gas
   | Water
   | Network
   | LocalNetwork
   | Rest
   | LocalRest
   deriving (Eq, Ord, Enum, Show)


data Base 

time =  (Strict.Axis {Strict.getLabel = "Time", 
                      Strict.getType = Type.T, 
                      Strict.getVec = [SignalFlow.TimeStep {SignalFlow.getMidTime = 1.0, SignalFlow.getTimeStep = 1.0},
                                       SignalFlow.TimeStep {SignalFlow.getMidTime = 2.0, SignalFlow.getTimeStep = 1.0},
                                       SignalFlow.TimeStep {SignalFlow.getMidTime = 3.0, SignalFlow.getTimeStep = 1.0}]}) 

rec1 :: SignalFlow.HRecord (TopoIdx.Position Node) Base String [] Double Double 
rec1 =  SignalFlow.HRecord time (Map.fromList [
                     (TopoIdx.Position LocalRest LocalNetwork,SignalFlow.Data {SignalFlow.getVector = [1.0,1.0,1.0]})])
       
rec2 :: SignalFlow.HRecord (TopoIdx.Position Node) Base String [] Double Double 
rec2 =  SignalFlow.HRecord time (Map.fromList [
                     (TopoIdx.Position LocalRest LocalNetwork,SignalFlow.Data {SignalFlow.getVector = [1.0,-1.0,1.0]})])

rec3 :: SignalFlow.HRecord (TopoIdx.Position Node) Base String [] Double Double 
rec3 =  SignalFlow.HRecord time (Map.fromList [
                     (TopoIdx.Position LocalRest LocalNetwork,SignalFlow.Data {SignalFlow.getVector = [1.0,1.0,-1.0]})])

rec4 :: SignalFlow.HRecord (TopoIdx.Position Node) Base String [] Double Double 
rec4 =  SignalFlow.HRecord time (Map.fromList [
                     (TopoIdx.Position LocalRest LocalNetwork,SignalFlow.Data {SignalFlow.getVector = [0.0,1.0,1.0]})])

--sequRec = Chop.chopHRecord (nc "Main") rec

main = do
  print $ Chop.findHSections (nc "Main") rec1
  print $ Chop.findHSections (nc "Main") rec2
  print $ Chop.findHSections (nc "Main") rec3
  print $ Chop.findHSections (nc "Main") rec4
--  print sequRec
  
{-  
  List [
    Section (Section 0) (Range (Idx {getInt = 0}) (Idx {getInt = 0})) 
    Flow.HRecord Axis {getLabel = "Time", getType = T, getVec = []} 
    fromList [(Position Coal Network,Data {getVector = [1.0]}),(Position LocalRest LocalNetwork,Data {getVector = [1.0]})],
    Section (Section 1) (Range (Idx {getInt = 1}) (Idx {getInt = 1})) 
    Flow.HRecord Axis {getLabel = "Time", getType = T, getVec = []} 
    fromList [(Position Coal Network,Data {getVector = [-2.0]}),(Position LocalRest LocalNetwork,Data {getVector = [1.0]})],
    Section (Section 2) (Range (Idx {getInt = 2}) (Idx {getInt = 2})) 
    Flow.HRecord Axis {getLabel = "Time", getType = T, getVec = []} 
    fromList [(Position Coal Network,Data {getVector = [3.0]}),(Position LocalRest LocalNetwork,Data {getVector = [1.0]})]]
-}