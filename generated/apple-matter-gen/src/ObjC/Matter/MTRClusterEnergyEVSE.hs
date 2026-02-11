{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Energy EVSE    Electric Vehicle Supply Equipment (EVSE) is equipment used to charge an Electric Vehicle (EV) or Plug-In Hybrid Electric Vehicle. This cluster provides an interface to the functionality of Electric Vehicle Supply Equipment (EVSE) management.
--
-- Generated bindings for @MTRClusterEnergyEVSE@.
module ObjC.Matter.MTRClusterEnergyEVSE
  ( MTRClusterEnergyEVSE
  , IsMTRClusterEnergyEVSE(..)
  , disableWithParams_expectedValues_expectedValueInterval_completion
  , disableWithExpectedValues_expectedValueInterval_completion
  , enableChargingWithParams_expectedValues_expectedValueInterval_completion
  , enableDischargingWithParams_expectedValues_expectedValueInterval_completion
  , startDiagnosticsWithParams_expectedValues_expectedValueInterval_completion
  , startDiagnosticsWithExpectedValues_expectedValueInterval_completion
  , setTargetsWithParams_expectedValues_expectedValueInterval_completion
  , getTargetsWithParams_expectedValues_expectedValueInterval_completion
  , getTargetsWithExpectedValues_expectedValueInterval_completion
  , clearTargetsWithParams_expectedValues_expectedValueInterval_completion
  , clearTargetsWithExpectedValues_expectedValueInterval_completion
  , readAttributeStateWithParams
  , readAttributeSupplyStateWithParams
  , readAttributeFaultStateWithParams
  , readAttributeChargingEnabledUntilWithParams
  , readAttributeDischargingEnabledUntilWithParams
  , readAttributeCircuitCapacityWithParams
  , readAttributeMinimumChargeCurrentWithParams
  , readAttributeMaximumChargeCurrentWithParams
  , readAttributeMaximumDischargeCurrentWithParams
  , readAttributeUserMaximumChargeCurrentWithParams
  , writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval
  , writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_params
  , readAttributeRandomizationDelayWindowWithParams
  , writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval
  , writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_params
  , readAttributeNextChargeStartTimeWithParams
  , readAttributeNextChargeTargetTimeWithParams
  , readAttributeNextChargeRequiredEnergyWithParams
  , readAttributeNextChargeTargetSoCWithParams
  , readAttributeApproximateEVEfficiencyWithParams
  , writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval
  , writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_params
  , readAttributeStateOfChargeWithParams
  , readAttributeBatteryCapacityWithParams
  , readAttributeVehicleIDWithParams
  , readAttributeSessionIDWithParams
  , readAttributeSessionDurationWithParams
  , readAttributeSessionEnergyChargedWithParams
  , readAttributeSessionEnergyDischargedWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , disableWithParams_expectedValues_expectedValueInterval_completionSelector
  , disableWithExpectedValues_expectedValueInterval_completionSelector
  , enableChargingWithParams_expectedValues_expectedValueInterval_completionSelector
  , enableDischargingWithParams_expectedValues_expectedValueInterval_completionSelector
  , startDiagnosticsWithParams_expectedValues_expectedValueInterval_completionSelector
  , startDiagnosticsWithExpectedValues_expectedValueInterval_completionSelector
  , setTargetsWithParams_expectedValues_expectedValueInterval_completionSelector
  , getTargetsWithParams_expectedValues_expectedValueInterval_completionSelector
  , getTargetsWithExpectedValues_expectedValueInterval_completionSelector
  , clearTargetsWithParams_expectedValues_expectedValueInterval_completionSelector
  , clearTargetsWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeStateWithParamsSelector
  , readAttributeSupplyStateWithParamsSelector
  , readAttributeFaultStateWithParamsSelector
  , readAttributeChargingEnabledUntilWithParamsSelector
  , readAttributeDischargingEnabledUntilWithParamsSelector
  , readAttributeCircuitCapacityWithParamsSelector
  , readAttributeMinimumChargeCurrentWithParamsSelector
  , readAttributeMaximumChargeCurrentWithParamsSelector
  , readAttributeMaximumDischargeCurrentWithParamsSelector
  , readAttributeUserMaximumChargeCurrentWithParamsSelector
  , writeAttributeUserMaximumChargeCurrentWithValue_expectedValueIntervalSelector
  , writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_paramsSelector
  , readAttributeRandomizationDelayWindowWithParamsSelector
  , writeAttributeRandomizationDelayWindowWithValue_expectedValueIntervalSelector
  , writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_paramsSelector
  , readAttributeNextChargeStartTimeWithParamsSelector
  , readAttributeNextChargeTargetTimeWithParamsSelector
  , readAttributeNextChargeRequiredEnergyWithParamsSelector
  , readAttributeNextChargeTargetSoCWithParamsSelector
  , readAttributeApproximateEVEfficiencyWithParamsSelector
  , writeAttributeApproximateEVEfficiencyWithValue_expectedValueIntervalSelector
  , writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_paramsSelector
  , readAttributeStateOfChargeWithParamsSelector
  , readAttributeBatteryCapacityWithParamsSelector
  , readAttributeVehicleIDWithParamsSelector
  , readAttributeSessionIDWithParamsSelector
  , readAttributeSessionDurationWithParamsSelector
  , readAttributeSessionEnergyChargedWithParamsSelector
  , readAttributeSessionEnergyDischargedWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpointID_queueSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- disableWithParams:expectedValues:expectedValueInterval:completion:@
disableWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterDisableParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
disableWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterEnergyEVSE (mkSelector "disableWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- disableWithExpectedValues:expectedValueInterval:completion:@
disableWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
disableWithExpectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterEnergyEVSE (mkSelector "disableWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enableChargingWithParams:expectedValues:expectedValueInterval:completion:@
enableChargingWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterEnableChargingParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableChargingWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterEnergyEVSE (mkSelector "enableChargingWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enableDischargingWithParams:expectedValues:expectedValueInterval:completion:@
enableDischargingWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterEnableDischargingParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableDischargingWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterEnergyEVSE (mkSelector "enableDischargingWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startDiagnosticsWithParams:expectedValues:expectedValueInterval:completion:@
startDiagnosticsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterStartDiagnosticsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startDiagnosticsWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterEnergyEVSE (mkSelector "startDiagnosticsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startDiagnosticsWithExpectedValues:expectedValueInterval:completion:@
startDiagnosticsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
startDiagnosticsWithExpectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterEnergyEVSE (mkSelector "startDiagnosticsWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setTargetsWithParams:expectedValues:expectedValueInterval:completion:@
setTargetsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterSetTargetsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTargetsWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterEnergyEVSE (mkSelector "setTargetsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getTargetsWithParams:expectedValues:expectedValueInterval:completion:@
getTargetsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterGetTargetsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getTargetsWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterEnergyEVSE (mkSelector "getTargetsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getTargetsWithExpectedValues:expectedValueInterval:completion:@
getTargetsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getTargetsWithExpectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterEnergyEVSE (mkSelector "getTargetsWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearTargetsWithParams:expectedValues:expectedValueInterval:completion:@
clearTargetsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterClearTargetsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearTargetsWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterEnergyEVSE (mkSelector "clearTargetsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearTargetsWithExpectedValues:expectedValueInterval:completion:@
clearTargetsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
clearTargetsWithExpectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterEnergyEVSE (mkSelector "clearTargetsWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStateWithParams:@
readAttributeStateWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeStateWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupplyStateWithParams:@
readAttributeSupplyStateWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeSupplyStateWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeSupplyStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFaultStateWithParams:@
readAttributeFaultStateWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeFaultStateWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeFaultStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeChargingEnabledUntilWithParams:@
readAttributeChargingEnabledUntilWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeChargingEnabledUntilWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeChargingEnabledUntilWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDischargingEnabledUntilWithParams:@
readAttributeDischargingEnabledUntilWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeDischargingEnabledUntilWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeDischargingEnabledUntilWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCircuitCapacityWithParams:@
readAttributeCircuitCapacityWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeCircuitCapacityWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeCircuitCapacityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinimumChargeCurrentWithParams:@
readAttributeMinimumChargeCurrentWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeMinimumChargeCurrentWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeMinimumChargeCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaximumChargeCurrentWithParams:@
readAttributeMaximumChargeCurrentWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeMaximumChargeCurrentWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeMaximumChargeCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaximumDischargeCurrentWithParams:@
readAttributeMaximumDischargeCurrentWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeMaximumDischargeCurrentWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeMaximumDischargeCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUserMaximumChargeCurrentWithParams:@
readAttributeUserMaximumChargeCurrentWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeUserMaximumChargeCurrentWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeUserMaximumChargeCurrentWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:@
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval mtrClusterEnergyEVSE  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterEnergyEVSE (mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:params:@
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_params :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_params mtrClusterEnergyEVSE  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterEnergyEVSE (mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeRandomizationDelayWindowWithParams:@
readAttributeRandomizationDelayWindowWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeRandomizationDelayWindowWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeRandomizationDelayWindowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:@
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval mtrClusterEnergyEVSE  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterEnergyEVSE (mkSelector "writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:params:@
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_params :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_params mtrClusterEnergyEVSE  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterEnergyEVSE (mkSelector "writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeNextChargeStartTimeWithParams:@
readAttributeNextChargeStartTimeWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeNextChargeStartTimeWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeNextChargeStartTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNextChargeTargetTimeWithParams:@
readAttributeNextChargeTargetTimeWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeNextChargeTargetTimeWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeNextChargeTargetTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNextChargeRequiredEnergyWithParams:@
readAttributeNextChargeRequiredEnergyWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeNextChargeRequiredEnergyWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeNextChargeRequiredEnergyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNextChargeTargetSoCWithParams:@
readAttributeNextChargeTargetSoCWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeNextChargeTargetSoCWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeNextChargeTargetSoCWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeApproximateEVEfficiencyWithParams:@
readAttributeApproximateEVEfficiencyWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeApproximateEVEfficiencyWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeApproximateEVEfficiencyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:@
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval mtrClusterEnergyEVSE  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterEnergyEVSE (mkSelector "writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:params:@
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_params :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_params mtrClusterEnergyEVSE  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterEnergyEVSE (mkSelector "writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeStateOfChargeWithParams:@
readAttributeStateOfChargeWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeStateOfChargeWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeStateOfChargeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatteryCapacityWithParams:@
readAttributeBatteryCapacityWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeBatteryCapacityWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeBatteryCapacityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeVehicleIDWithParams:@
readAttributeVehicleIDWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeVehicleIDWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeVehicleIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSessionIDWithParams:@
readAttributeSessionIDWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeSessionIDWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeSessionIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSessionDurationWithParams:@
readAttributeSessionDurationWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeSessionDurationWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeSessionDurationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSessionEnergyChargedWithParams:@
readAttributeSessionEnergyChargedWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeSessionEnergyChargedWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeSessionEnergyChargedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSessionEnergyDischargedWithParams:@
readAttributeSessionEnergyDischargedWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeSessionEnergyDischargedWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeSessionEnergyDischargedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterEnergyEVSE  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEnergyEVSE (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE => mtrClusterEnergyEVSE -> IO (Id MTRClusterEnergyEVSE)
init_ mtrClusterEnergyEVSE  =
    sendMsg mtrClusterEnergyEVSE (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterEnergyEVSE)
new  =
  do
    cls' <- getRequiredClass "MTRClusterEnergyEVSE"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterEnergyEVSE -> device -> endpointID -> queue -> IO (Id MTRClusterEnergyEVSE)
initWithDevice_endpointID_queue mtrClusterEnergyEVSE  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterEnergyEVSE (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disableWithParams:expectedValues:expectedValueInterval:completion:@
disableWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
disableWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "disableWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @disableWithExpectedValues:expectedValueInterval:completion:@
disableWithExpectedValues_expectedValueInterval_completionSelector :: Selector
disableWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "disableWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableChargingWithParams:expectedValues:expectedValueInterval:completion:@
enableChargingWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
enableChargingWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enableChargingWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableDischargingWithParams:expectedValues:expectedValueInterval:completion:@
enableDischargingWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
enableDischargingWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enableDischargingWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startDiagnosticsWithParams:expectedValues:expectedValueInterval:completion:@
startDiagnosticsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
startDiagnosticsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "startDiagnosticsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startDiagnosticsWithExpectedValues:expectedValueInterval:completion:@
startDiagnosticsWithExpectedValues_expectedValueInterval_completionSelector :: Selector
startDiagnosticsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "startDiagnosticsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTargetsWithParams:expectedValues:expectedValueInterval:completion:@
setTargetsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setTargetsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTargetsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getTargetsWithParams:expectedValues:expectedValueInterval:completion:@
getTargetsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getTargetsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getTargetsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getTargetsWithExpectedValues:expectedValueInterval:completion:@
getTargetsWithExpectedValues_expectedValueInterval_completionSelector :: Selector
getTargetsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "getTargetsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearTargetsWithParams:expectedValues:expectedValueInterval:completion:@
clearTargetsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
clearTargetsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "clearTargetsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearTargetsWithExpectedValues:expectedValueInterval:completion:@
clearTargetsWithExpectedValues_expectedValueInterval_completionSelector :: Selector
clearTargetsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "clearTargetsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeStateWithParams:@
readAttributeStateWithParamsSelector :: Selector
readAttributeStateWithParamsSelector = mkSelector "readAttributeStateWithParams:"

-- | @Selector@ for @readAttributeSupplyStateWithParams:@
readAttributeSupplyStateWithParamsSelector :: Selector
readAttributeSupplyStateWithParamsSelector = mkSelector "readAttributeSupplyStateWithParams:"

-- | @Selector@ for @readAttributeFaultStateWithParams:@
readAttributeFaultStateWithParamsSelector :: Selector
readAttributeFaultStateWithParamsSelector = mkSelector "readAttributeFaultStateWithParams:"

-- | @Selector@ for @readAttributeChargingEnabledUntilWithParams:@
readAttributeChargingEnabledUntilWithParamsSelector :: Selector
readAttributeChargingEnabledUntilWithParamsSelector = mkSelector "readAttributeChargingEnabledUntilWithParams:"

-- | @Selector@ for @readAttributeDischargingEnabledUntilWithParams:@
readAttributeDischargingEnabledUntilWithParamsSelector :: Selector
readAttributeDischargingEnabledUntilWithParamsSelector = mkSelector "readAttributeDischargingEnabledUntilWithParams:"

-- | @Selector@ for @readAttributeCircuitCapacityWithParams:@
readAttributeCircuitCapacityWithParamsSelector :: Selector
readAttributeCircuitCapacityWithParamsSelector = mkSelector "readAttributeCircuitCapacityWithParams:"

-- | @Selector@ for @readAttributeMinimumChargeCurrentWithParams:@
readAttributeMinimumChargeCurrentWithParamsSelector :: Selector
readAttributeMinimumChargeCurrentWithParamsSelector = mkSelector "readAttributeMinimumChargeCurrentWithParams:"

-- | @Selector@ for @readAttributeMaximumChargeCurrentWithParams:@
readAttributeMaximumChargeCurrentWithParamsSelector :: Selector
readAttributeMaximumChargeCurrentWithParamsSelector = mkSelector "readAttributeMaximumChargeCurrentWithParams:"

-- | @Selector@ for @readAttributeMaximumDischargeCurrentWithParams:@
readAttributeMaximumDischargeCurrentWithParamsSelector :: Selector
readAttributeMaximumDischargeCurrentWithParamsSelector = mkSelector "readAttributeMaximumDischargeCurrentWithParams:"

-- | @Selector@ for @readAttributeUserMaximumChargeCurrentWithParams:@
readAttributeUserMaximumChargeCurrentWithParamsSelector :: Selector
readAttributeUserMaximumChargeCurrentWithParamsSelector = mkSelector "readAttributeUserMaximumChargeCurrentWithParams:"

-- | @Selector@ for @writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:@
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueIntervalSelector :: Selector
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:params:@
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRandomizationDelayWindowWithParams:@
readAttributeRandomizationDelayWindowWithParamsSelector :: Selector
readAttributeRandomizationDelayWindowWithParamsSelector = mkSelector "readAttributeRandomizationDelayWindowWithParams:"

-- | @Selector@ for @writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:@
writeAttributeRandomizationDelayWindowWithValue_expectedValueIntervalSelector :: Selector
writeAttributeRandomizationDelayWindowWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:params:@
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeNextChargeStartTimeWithParams:@
readAttributeNextChargeStartTimeWithParamsSelector :: Selector
readAttributeNextChargeStartTimeWithParamsSelector = mkSelector "readAttributeNextChargeStartTimeWithParams:"

-- | @Selector@ for @readAttributeNextChargeTargetTimeWithParams:@
readAttributeNextChargeTargetTimeWithParamsSelector :: Selector
readAttributeNextChargeTargetTimeWithParamsSelector = mkSelector "readAttributeNextChargeTargetTimeWithParams:"

-- | @Selector@ for @readAttributeNextChargeRequiredEnergyWithParams:@
readAttributeNextChargeRequiredEnergyWithParamsSelector :: Selector
readAttributeNextChargeRequiredEnergyWithParamsSelector = mkSelector "readAttributeNextChargeRequiredEnergyWithParams:"

-- | @Selector@ for @readAttributeNextChargeTargetSoCWithParams:@
readAttributeNextChargeTargetSoCWithParamsSelector :: Selector
readAttributeNextChargeTargetSoCWithParamsSelector = mkSelector "readAttributeNextChargeTargetSoCWithParams:"

-- | @Selector@ for @readAttributeApproximateEVEfficiencyWithParams:@
readAttributeApproximateEVEfficiencyWithParamsSelector :: Selector
readAttributeApproximateEVEfficiencyWithParamsSelector = mkSelector "readAttributeApproximateEVEfficiencyWithParams:"

-- | @Selector@ for @writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:@
writeAttributeApproximateEVEfficiencyWithValue_expectedValueIntervalSelector :: Selector
writeAttributeApproximateEVEfficiencyWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:params:@
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeStateOfChargeWithParams:@
readAttributeStateOfChargeWithParamsSelector :: Selector
readAttributeStateOfChargeWithParamsSelector = mkSelector "readAttributeStateOfChargeWithParams:"

-- | @Selector@ for @readAttributeBatteryCapacityWithParams:@
readAttributeBatteryCapacityWithParamsSelector :: Selector
readAttributeBatteryCapacityWithParamsSelector = mkSelector "readAttributeBatteryCapacityWithParams:"

-- | @Selector@ for @readAttributeVehicleIDWithParams:@
readAttributeVehicleIDWithParamsSelector :: Selector
readAttributeVehicleIDWithParamsSelector = mkSelector "readAttributeVehicleIDWithParams:"

-- | @Selector@ for @readAttributeSessionIDWithParams:@
readAttributeSessionIDWithParamsSelector :: Selector
readAttributeSessionIDWithParamsSelector = mkSelector "readAttributeSessionIDWithParams:"

-- | @Selector@ for @readAttributeSessionDurationWithParams:@
readAttributeSessionDurationWithParamsSelector :: Selector
readAttributeSessionDurationWithParamsSelector = mkSelector "readAttributeSessionDurationWithParams:"

-- | @Selector@ for @readAttributeSessionEnergyChargedWithParams:@
readAttributeSessionEnergyChargedWithParamsSelector :: Selector
readAttributeSessionEnergyChargedWithParamsSelector = mkSelector "readAttributeSessionEnergyChargedWithParams:"

-- | @Selector@ for @readAttributeSessionEnergyDischargedWithParams:@
readAttributeSessionEnergyDischargedWithParamsSelector :: Selector
readAttributeSessionEnergyDischargedWithParamsSelector = mkSelector "readAttributeSessionEnergyDischargedWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

