{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module CoreRun
  ( runCore
  ) where

import qualified Clash.Prelude as C
import Core.CPU (CPUIn (..), CPUOut (..), mealyCore)
import Core.Concrete.ReduceRulesLambda (AgentSimpleLambda)
import Core.Loader (delayedMaybeRam)
import Core.MemoryManager.MemoryManager (MemoryManager)
import Core.Node (AddressNumber, CellsNumber, LoadedNode (..), Node)
import qualified Data.Map.Strict as M
import NetSnapshot (NetSnapshot (..), fromVec)

type Cells = C.Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))

outputs
  :: (C.KnownDomain dom, C.HiddenClockResetEnable dom)
  => Cells
  -> AddressNumber
  -> MemoryManager
  -> C.Signal dom (CPUOut 2 AgentSimpleLambda)
outputs net root memoryManager = o
 where
  ram = C.exposeEnable (C.blockRam net)
  i = C.register (CPUIn Nothing) nextInput
  o = mealyCore @2 @2 @2 @AgentSimpleLambda memoryManager root i
  nextLoadedNode = delayedMaybeRam ram (_ramForm <$> o)
  nextInput = CPUIn <$> (loaded <$> nextLoadedNode <*> (_ramForm <$> o))
  loaded n ramForm = LoadedNode <$> n <*> (fst <$> ramForm)

runCore :: Int -> Cells -> AddressNumber -> MemoryManager -> NetSnapshot 2 AgentSimpleLambda
runCore cycles net root memoryManager =
  NetSnapshot (_nextRootNodeAddress (last trace)) (foldl write (snapNodes (fromVec root net)) trace)
 where
  trace = C.sampleN cycles (outputs @C.System net root memoryManager)
  write nodes out = case _ramForm out of
    Just (_, Just (a, Just n)) -> M.insert a n nodes
    Just (_, Just (a, Nothing)) -> M.delete a nodes
    _ -> nodes