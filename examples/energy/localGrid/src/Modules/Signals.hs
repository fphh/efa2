{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}


module Modules.Signals where

import Modules.System (Node(..))

import qualified EFA.Application.Index as XIdx

import EFA.Signal.Record (SigId(SigId),
                          getSig,getTime,extractLogSignals,
                          PowerRecord, SignalRecord, genPowerRecord)

import EFA.Signal.Signal (neg)


---------------------------------------------------------------------------------------
-- * Model the System Topology

condition :: SignalRecord [] Double -> SignalRecord [] Double
condition rec = extractLogSignals rec
                [(SigId "coalpowerplant1.powersourcedemand1._current_log",neg),
                 (SigId "coalpowerplant1.powersourcedemand1._powerDemand_log", id),
                 (SigId "coalpowerplant1.powersourcedemand1._powerElectric_log", neg),
                 (SigId "coalpowerplant1.powersourcedemand1._powerIn_log", id),
                 (SigId "coalpowerplant1.powersourcedemand1._voltage_log", id),
                 (SigId "coalpowerplant1.powersourcedemand1.poweroutefficiency1._eta_log", id),
                 (SigId "coalpowerplant1.powersourcedemand1.poweroutefficiency1._powerIn_log", id),
                 (SigId "coalpowerplant1.powersourcedemand1.poweroutefficiency1._powerOut_log", id),
                 (SigId "gaspowerplant1.powersourcedemand1._current_log", neg),
                 (SigId "gaspowerplant1.powersourcedemand1._powerDemand_log", id),
                 (SigId "gaspowerplant1.powersourcedemand1._powerElectric_log", neg),
                 (SigId "gaspowerplant1.powersourcedemand1._powerIn_log", id),
                 (SigId "gaspowerplant1.powersourcedemand1._voltage_log", id),
                 (SigId "gaspowerplant1.powersourcedemand1.poweroutefficiency1._eta_log", id),
                 (SigId "gaspowerplant1.powersourcedemand1.poweroutefficiency1._powerIn_log", id),
                 (SigId "gaspowerplant1.powersourcedemand1.poweroutefficiency1._powerOut_log", id),
                 (SigId "household1.powersink1._current_log", id),
                 (SigId "household1.powersink1._powerDemand_log", id),
                 (SigId "household1.powersink1._power_log", id),
                 (SigId "household1.powersink1._voltage_log", id),
                 (SigId "industry1.powersink1._current_log", id),
                 (SigId "industry1.powersink1._powerDemand_log", id),
                 (SigId "industry1.powersink1._power_log", id),
                 (SigId "industry1.powersink1._voltage_log", id),
                 (SigId "solar1.powersourcesupply1._current_log", neg),
                 (SigId "solar1.powersourcesupply1._powerElectric_log", neg),
                 (SigId "solar1.powersourcesupply1._powerSupply_log", id),
                 (SigId "solar1.powersourcesupply1._voltage_log", id),
                 (SigId "solar1.powersourcesupply1.powerinefficiency1._eta_log", id),
                 (SigId "solar1.powersourcesupply1.powerinefficiency1._powerIn_log", id),
                 (SigId "solar1.powersourcesupply1.powerinefficiency1._powerOut_log", id),
                 (SigId "storageplant1.storage1._current_log", neg),
                 (SigId "storageplant1.storage1._powerExternal_log", neg),
                 (SigId "storageplant1.storage1._powerInternal_log", neg),
                 (SigId "storageplant1.storage1._voltageInternal_log", id),
                 (SigId "storageplant1.storage1._voltage_log", id),
                 (SigId "storageplant1.storage1.electricefficiency1._current1_log", id),
                 (SigId "storageplant1.storage1.electricefficiency1._current2_log", id),
                 (SigId "storageplant1.storage1.electricefficiency1._powerElectric1_log", id),
                 (SigId "storageplant1.storage1.electricefficiency1._powerElectric2_log", id),
                 (SigId "storageplant1.storage1.electricefficiency1._voltage1_log", id),
                 (SigId "storageplant1.storage1.electricefficiency1._voltage2_log", id),
                 (SigId "transformer1.electricefficiency1._current1_log", id),
                 (SigId "transformer1.electricefficiency1._current2_log", neg),
                 (SigId "transformer1.electricefficiency1._powerElectric1_log", id),
                 (SigId "transformer1.electricefficiency1._powerElectric2_log", neg),
                 (SigId "transformer1.electricefficiency1._voltage1_log", id),
                 (SigId "transformer1.electricefficiency1._voltage2_log", id),
                 (SigId "wind1.powersourcesupply1._current_log", id),
                 (SigId "wind1.powersourcesupply1._powerElectric_log", neg),
                 (SigId "wind1.powersourcesupply1._powerSupply_log", id),
                 (SigId "wind1.powersourcesupply1._voltage_log", id),
                 (SigId "wind1.powersourcesupply1.powerinefficiency1._eta_log", id),
                 (SigId "wind1.powersourcesupply1.powerinefficiency1._powerIn_log", id),
                 (SigId "wind1.powersourcesupply1.powerinefficiency1._powerOut_log", id)]


---------------------------------------------------------------------------------------
-- * Calculate special signals
calculatePower :: SignalRecord [] Double -> PowerRecord Node [] Double
calculatePower rec = pRec
  where
      -- Convenience function
      g sigId = getSig rec $ SigId sigId
      time = getTime rec
      pRec = genPowerRecord time
              -- engine
             [(XIdx.ppos Coal Network,
               g "coalpowerplant1.powersourcedemand1._powerIn_log",
               g "coalpowerplant1.powersourcedemand1._powerElectric_log"
              ),

              -- connection
              (XIdx.ppos Gas Network,
               g "gaspowerplant1.powersourcedemand1._powerIn_log",
               g "gaspowerplant1.powersourcedemand1._powerElectric_log"
              ),

              (XIdx.ppos Water Network,
               g "storageplant1.storage1._powerInternal_log",
               g "storageplant1.storage1._powerExternal_log"
              ),

              (XIdx.ppos Network LocalNetwork,
               g "transformer1.electricefficiency1._powerElectric1_log",
               g "transformer1.electricefficiency1._powerElectric2_log"
              ),

              (XIdx.ppos Wind Network,
               g "wind1.powersourcesupply1._powerSupply_log",
               g "wind1.powersourcesupply1._powerElectric_log"
               ),

              (XIdx.ppos Sun LocalNetwork,
               g "solar1.powersourcesupply1._powerSupply_log",
               g "solar1.powersourcesupply1._powerElectric_log"
              ),

              (XIdx.ppos HouseHold LocalNetwork,
               g "household1.powersink1._powerDemand_log",
               g "household1.powersink1._power_log"
               ),

              (XIdx.ppos Industry LocalNetwork,
               g "industry1.powersink1._powerDemand_log",
               g "industry1.powersink1._power_log"
              )]


---------------------------------------------------------------------------------------
-- ## Signalgroups for Plotting

coal :: [SigId]
coal = [SigId "coalpowerplant1.powersourcedemand1._current_log",
        SigId "coalpowerplant1.powersourcedemand1._powerDemand_log",
        SigId "coalpowerplant1.powersourcedemand1._powerElectric_log",
        SigId "coalpowerplant1.powersourcedemand1._powerIn_log",
        SigId "coalpowerplant1.powersourcedemand1._voltage_log",
        SigId "coalpowerplant1.powersourcedemand1.poweroutefficiency1._eta_log",
        SigId "coalpowerplant1.powersourcedemand1.poweroutefficiency1._powerIn_log",
        SigId "coalpowerplant1.powersourcedemand1.poweroutefficiency1._powerOut_log"]

gas :: [SigId]
gas = [SigId "gaspowerplant1.powersourcedemand1._current_log",
       SigId "gaspowerplant1.powersourcedemand1._powerDemand_log",
       SigId "gaspowerplant1.powersourcedemand1._powerElectric_log",
       SigId "gaspowerplant1.powersourcedemand1._powerIn_log",
       SigId "gaspowerplant1.powersourcedemand1._voltage_log",
       SigId "gaspowerplant1.powersourcedemand1.poweroutefficiency1._eta_log",
       SigId "gaspowerplant1.powersourcedemand1.poweroutefficiency1._powerIn_log",
       SigId "gaspowerplant1.powersourcedemand1.poweroutefficiency1._powerOut_log"]

house :: [SigId]
house = [SigId "household1.powersink1._current_log",
         SigId "household1.powersink1._powerDemand_log",
         SigId "household1.powersink1._power_log",
         SigId "household1.powersink1._voltage_log"]

industry ::  [SigId]
industry = [SigId "industry1.powersink1._current_log",
            SigId "industry1.powersink1._powerDemand_log",
            SigId "industry1.powersink1._power_log",
            SigId "industry1.powersink1._voltage_log"]

solar ::[SigId]
solar = [SigId "solar1.powersourcesupply1._current_log",
         SigId "solar1.powersourcesupply1._powerElectric_log",
         SigId "solar1.powersourcesupply1._powerSupply_log",
         SigId "solar1.powersourcesupply1._voltage_log",
         SigId "solar1.powersourcesupply1.powerinefficiency1._eta_log",
         SigId "solar1.powersourcesupply1.powerinefficiency1._powerIn_log",
         SigId "solar1.powersourcesupply1.powerinefficiency1._powerOut_log"]

water :: [SigId]
water = [SigId "storageplant1.storage1._current_log",
           SigId "storageplant1.storage1._powerExternal_log",
           SigId "storageplant1.storage1._powerInternal_log",
           SigId "storageplant1.storage1._voltageInternal_log",
           SigId "storageplant1.storage1._voltage_log",
           SigId "storageplant1.storage1.electricefficiency1._current1_log",
           SigId "storageplant1.storage1.electricefficiency1._current2_log",
           SigId "storageplant1.storage1.electricefficiency1._powerElectric1_log",
           SigId "storageplant1.storage1.electricefficiency1._powerElectric2_log",
           SigId "storageplant1.storage1.electricefficiency1._voltage1_log",
           SigId "storageplant1.storage1.electricefficiency1._voltage2_log"]

transformer :: [SigId]
transformer = [SigId "transformer1.electricefficiency1._current1_log",
               SigId "transformer1.electricefficiency1._current2_log",
               SigId "transformer1.electricefficiency1._powerElectric1_log",
               SigId "transformer1.electricefficiency1._powerElectric2_log",
               SigId "transformer1.electricefficiency1._voltage1_log",
               SigId "transformer1.electricefficiency1._voltage2_log"]

wind :: [SigId]
wind = [SigId "wind1.powersourcesupply1._current_log",
        SigId "wind1.powersourcesupply1._powerElectric_log",
        SigId "wind1.powersourcesupply1._powerSupply_log",
        SigId "wind1.powersourcesupply1._voltage_log",
        SigId "wind1.powersourcesupply1.powerinefficiency1._eta_log",
        SigId "wind1.powersourcesupply1.powerinefficiency1._powerIn_log",
        SigId "wind1.powersourcesupply1.powerinefficiency1._powerOut_log"]



{-
xyEngine :: (SigId,SigId)
xyEngine = (SigId "engine1._speed_log", SigId "engine1._torque_log")

xyGenerator :: (SigId,SigId)
xyGenerator = (SigId "engine1._speed_log", SigId "generator._torque_log")

xyMotor :: (SigId,SigId)
xyMotor = (SigId "motor._speed_log", SigId "motor._torque_log")
-}

etaWind :: (XIdx.PPos Node,XIdx.PPos Node,XIdx.PPos Node)
etaWind = (XIdx.ppos Wind Network, XIdx.ppos Network Wind, XIdx.ppos Wind Network)

etaSolar :: (XIdx.PPos Node,XIdx.PPos Node,XIdx.PPos Node)
etaSolar = (XIdx.ppos Sun LocalNetwork, XIdx.ppos LocalNetwork Sun, XIdx.ppos Sun LocalNetwork)

etaWater :: (XIdx.PPos Node,XIdx.PPos Node,XIdx.PPos Node)
etaWater = (XIdx.ppos Water Network, XIdx.ppos Network Water, XIdx.ppos Water Network)

etaGas :: (XIdx.PPos Node,XIdx.PPos Node,XIdx.PPos Node)
etaGas = (XIdx.ppos Gas Network, XIdx.ppos Network Gas, XIdx.ppos Gas Network)

etaCoal :: (XIdx.PPos Node,XIdx.PPos Node,XIdx.PPos Node)
etaCoal = (XIdx.ppos Coal Network, XIdx.ppos Network Coal, XIdx.ppos Coal Network)

etaTransformer :: (XIdx.PPos Node,XIdx.PPos Node,XIdx.PPos Node)
etaTransformer = (XIdx.ppos Network LocalNetwork, XIdx.ppos LocalNetwork Network, XIdx.ppos Network LocalNetwork)

