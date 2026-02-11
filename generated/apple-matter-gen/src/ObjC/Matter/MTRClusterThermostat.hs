{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thermostat    An interface for configuring and controlling the functionality of a thermostat.
--
-- Generated bindings for @MTRClusterThermostat@.
module ObjC.Matter.MTRClusterThermostat
  ( MTRClusterThermostat
  , IsMTRClusterThermostat(..)
  , setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completion
  , setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completion
  , getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completion
  , clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completion
  , clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completion
  , setActiveScheduleRequestWithParams_expectedValues_expectedValueInterval_completion
  , setActivePresetRequestWithParams_expectedValues_expectedValueInterval_completion
  , addThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completion
  , removeThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completion
  , atomicRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeLocalTemperatureWithParams
  , readAttributeOutdoorTemperatureWithParams
  , readAttributeOccupancyWithParams
  , readAttributeAbsMinHeatSetpointLimitWithParams
  , readAttributeAbsMaxHeatSetpointLimitWithParams
  , readAttributeAbsMinCoolSetpointLimitWithParams
  , readAttributeAbsMaxCoolSetpointLimitWithParams
  , readAttributePICoolingDemandWithParams
  , readAttributePIHeatingDemandWithParams
  , readAttributeHVACSystemTypeConfigurationWithParams
  , writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueInterval
  , writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueInterval_params
  , readAttributeLocalTemperatureCalibrationWithParams
  , writeAttributeLocalTemperatureCalibrationWithValue_expectedValueInterval
  , writeAttributeLocalTemperatureCalibrationWithValue_expectedValueInterval_params
  , readAttributeOccupiedCoolingSetpointWithParams
  , writeAttributeOccupiedCoolingSetpointWithValue_expectedValueInterval
  , writeAttributeOccupiedCoolingSetpointWithValue_expectedValueInterval_params
  , readAttributeOccupiedHeatingSetpointWithParams
  , writeAttributeOccupiedHeatingSetpointWithValue_expectedValueInterval
  , writeAttributeOccupiedHeatingSetpointWithValue_expectedValueInterval_params
  , readAttributeUnoccupiedCoolingSetpointWithParams
  , writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueInterval
  , writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueInterval_params
  , readAttributeUnoccupiedHeatingSetpointWithParams
  , writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueInterval
  , writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueInterval_params
  , readAttributeMinHeatSetpointLimitWithParams
  , writeAttributeMinHeatSetpointLimitWithValue_expectedValueInterval
  , writeAttributeMinHeatSetpointLimitWithValue_expectedValueInterval_params
  , readAttributeMaxHeatSetpointLimitWithParams
  , writeAttributeMaxHeatSetpointLimitWithValue_expectedValueInterval
  , writeAttributeMaxHeatSetpointLimitWithValue_expectedValueInterval_params
  , readAttributeMinCoolSetpointLimitWithParams
  , writeAttributeMinCoolSetpointLimitWithValue_expectedValueInterval
  , writeAttributeMinCoolSetpointLimitWithValue_expectedValueInterval_params
  , readAttributeMaxCoolSetpointLimitWithParams
  , writeAttributeMaxCoolSetpointLimitWithValue_expectedValueInterval
  , writeAttributeMaxCoolSetpointLimitWithValue_expectedValueInterval_params
  , readAttributeMinSetpointDeadBandWithParams
  , writeAttributeMinSetpointDeadBandWithValue_expectedValueInterval
  , writeAttributeMinSetpointDeadBandWithValue_expectedValueInterval_params
  , readAttributeRemoteSensingWithParams
  , writeAttributeRemoteSensingWithValue_expectedValueInterval
  , writeAttributeRemoteSensingWithValue_expectedValueInterval_params
  , readAttributeControlSequenceOfOperationWithParams
  , writeAttributeControlSequenceOfOperationWithValue_expectedValueInterval
  , writeAttributeControlSequenceOfOperationWithValue_expectedValueInterval_params
  , readAttributeSystemModeWithParams
  , writeAttributeSystemModeWithValue_expectedValueInterval
  , writeAttributeSystemModeWithValue_expectedValueInterval_params
  , readAttributeThermostatRunningModeWithParams
  , readAttributeStartOfWeekWithParams
  , readAttributeNumberOfWeeklyTransitionsWithParams
  , readAttributeNumberOfDailyTransitionsWithParams
  , readAttributeTemperatureSetpointHoldWithParams
  , writeAttributeTemperatureSetpointHoldWithValue_expectedValueInterval
  , writeAttributeTemperatureSetpointHoldWithValue_expectedValueInterval_params
  , readAttributeTemperatureSetpointHoldDurationWithParams
  , writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueInterval
  , writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueInterval_params
  , readAttributeThermostatProgrammingOperationModeWithParams
  , writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueInterval
  , writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueInterval_params
  , readAttributeThermostatRunningStateWithParams
  , readAttributeSetpointChangeSourceWithParams
  , readAttributeSetpointChangeAmountWithParams
  , readAttributeSetpointChangeSourceTimestampWithParams
  , readAttributeOccupiedSetbackWithParams
  , writeAttributeOccupiedSetbackWithValue_expectedValueInterval
  , writeAttributeOccupiedSetbackWithValue_expectedValueInterval_params
  , readAttributeOccupiedSetbackMinWithParams
  , readAttributeOccupiedSetbackMaxWithParams
  , readAttributeUnoccupiedSetbackWithParams
  , writeAttributeUnoccupiedSetbackWithValue_expectedValueInterval
  , writeAttributeUnoccupiedSetbackWithValue_expectedValueInterval_params
  , readAttributeUnoccupiedSetbackMinWithParams
  , readAttributeUnoccupiedSetbackMaxWithParams
  , readAttributeEmergencyHeatDeltaWithParams
  , writeAttributeEmergencyHeatDeltaWithValue_expectedValueInterval
  , writeAttributeEmergencyHeatDeltaWithValue_expectedValueInterval_params
  , readAttributeACTypeWithParams
  , writeAttributeACTypeWithValue_expectedValueInterval
  , writeAttributeACTypeWithValue_expectedValueInterval_params
  , readAttributeACCapacityWithParams
  , writeAttributeACCapacityWithValue_expectedValueInterval
  , writeAttributeACCapacityWithValue_expectedValueInterval_params
  , readAttributeACRefrigerantTypeWithParams
  , writeAttributeACRefrigerantTypeWithValue_expectedValueInterval
  , writeAttributeACRefrigerantTypeWithValue_expectedValueInterval_params
  , readAttributeACCompressorTypeWithParams
  , writeAttributeACCompressorTypeWithValue_expectedValueInterval
  , writeAttributeACCompressorTypeWithValue_expectedValueInterval_params
  , readAttributeACErrorCodeWithParams
  , writeAttributeACErrorCodeWithValue_expectedValueInterval
  , writeAttributeACErrorCodeWithValue_expectedValueInterval_params
  , readAttributeACLouverPositionWithParams
  , writeAttributeACLouverPositionWithValue_expectedValueInterval
  , writeAttributeACLouverPositionWithValue_expectedValueInterval_params
  , readAttributeACCoilTemperatureWithParams
  , readAttributeACCapacityformatWithParams
  , writeAttributeACCapacityformatWithValue_expectedValueInterval
  , writeAttributeACCapacityformatWithValue_expectedValueInterval_params
  , readAttributePresetTypesWithParams
  , readAttributeScheduleTypesWithParams
  , readAttributeNumberOfPresetsWithParams
  , readAttributeNumberOfSchedulesWithParams
  , readAttributeNumberOfScheduleTransitionsWithParams
  , readAttributeNumberOfScheduleTransitionPerDayWithParams
  , readAttributeActivePresetHandleWithParams
  , readAttributeActiveScheduleHandleWithParams
  , readAttributePresetsWithParams
  , writeAttributePresetsWithValue_expectedValueInterval
  , writeAttributePresetsWithValue_expectedValueInterval_params
  , readAttributeSchedulesWithParams
  , writeAttributeSchedulesWithValue_expectedValueInterval
  , writeAttributeSchedulesWithValue_expectedValueInterval_params
  , readAttributeSetpointHoldExpiryTimestampWithParams
  , readAttributeMaxThermostatSuggestionsWithParams
  , readAttributeThermostatSuggestionsWithParams
  , readAttributeCurrentThermostatSuggestionWithParams
  , readAttributeThermostatSuggestionNotFollowingReasonWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completionHandler
  , setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandler
  , clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completionSelector
  , setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionSelector
  , clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completionSelector
  , setActiveScheduleRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , setActivePresetRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , addThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completionSelector
  , atomicRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeLocalTemperatureWithParamsSelector
  , readAttributeOutdoorTemperatureWithParamsSelector
  , readAttributeOccupancyWithParamsSelector
  , readAttributeAbsMinHeatSetpointLimitWithParamsSelector
  , readAttributeAbsMaxHeatSetpointLimitWithParamsSelector
  , readAttributeAbsMinCoolSetpointLimitWithParamsSelector
  , readAttributeAbsMaxCoolSetpointLimitWithParamsSelector
  , readAttributePICoolingDemandWithParamsSelector
  , readAttributePIHeatingDemandWithParamsSelector
  , readAttributeHVACSystemTypeConfigurationWithParamsSelector
  , writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueIntervalSelector
  , writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueInterval_paramsSelector
  , readAttributeLocalTemperatureCalibrationWithParamsSelector
  , writeAttributeLocalTemperatureCalibrationWithValue_expectedValueIntervalSelector
  , writeAttributeLocalTemperatureCalibrationWithValue_expectedValueInterval_paramsSelector
  , readAttributeOccupiedCoolingSetpointWithParamsSelector
  , writeAttributeOccupiedCoolingSetpointWithValue_expectedValueIntervalSelector
  , writeAttributeOccupiedCoolingSetpointWithValue_expectedValueInterval_paramsSelector
  , readAttributeOccupiedHeatingSetpointWithParamsSelector
  , writeAttributeOccupiedHeatingSetpointWithValue_expectedValueIntervalSelector
  , writeAttributeOccupiedHeatingSetpointWithValue_expectedValueInterval_paramsSelector
  , readAttributeUnoccupiedCoolingSetpointWithParamsSelector
  , writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueIntervalSelector
  , writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueInterval_paramsSelector
  , readAttributeUnoccupiedHeatingSetpointWithParamsSelector
  , writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueIntervalSelector
  , writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueInterval_paramsSelector
  , readAttributeMinHeatSetpointLimitWithParamsSelector
  , writeAttributeMinHeatSetpointLimitWithValue_expectedValueIntervalSelector
  , writeAttributeMinHeatSetpointLimitWithValue_expectedValueInterval_paramsSelector
  , readAttributeMaxHeatSetpointLimitWithParamsSelector
  , writeAttributeMaxHeatSetpointLimitWithValue_expectedValueIntervalSelector
  , writeAttributeMaxHeatSetpointLimitWithValue_expectedValueInterval_paramsSelector
  , readAttributeMinCoolSetpointLimitWithParamsSelector
  , writeAttributeMinCoolSetpointLimitWithValue_expectedValueIntervalSelector
  , writeAttributeMinCoolSetpointLimitWithValue_expectedValueInterval_paramsSelector
  , readAttributeMaxCoolSetpointLimitWithParamsSelector
  , writeAttributeMaxCoolSetpointLimitWithValue_expectedValueIntervalSelector
  , writeAttributeMaxCoolSetpointLimitWithValue_expectedValueInterval_paramsSelector
  , readAttributeMinSetpointDeadBandWithParamsSelector
  , writeAttributeMinSetpointDeadBandWithValue_expectedValueIntervalSelector
  , writeAttributeMinSetpointDeadBandWithValue_expectedValueInterval_paramsSelector
  , readAttributeRemoteSensingWithParamsSelector
  , writeAttributeRemoteSensingWithValue_expectedValueIntervalSelector
  , writeAttributeRemoteSensingWithValue_expectedValueInterval_paramsSelector
  , readAttributeControlSequenceOfOperationWithParamsSelector
  , writeAttributeControlSequenceOfOperationWithValue_expectedValueIntervalSelector
  , writeAttributeControlSequenceOfOperationWithValue_expectedValueInterval_paramsSelector
  , readAttributeSystemModeWithParamsSelector
  , writeAttributeSystemModeWithValue_expectedValueIntervalSelector
  , writeAttributeSystemModeWithValue_expectedValueInterval_paramsSelector
  , readAttributeThermostatRunningModeWithParamsSelector
  , readAttributeStartOfWeekWithParamsSelector
  , readAttributeNumberOfWeeklyTransitionsWithParamsSelector
  , readAttributeNumberOfDailyTransitionsWithParamsSelector
  , readAttributeTemperatureSetpointHoldWithParamsSelector
  , writeAttributeTemperatureSetpointHoldWithValue_expectedValueIntervalSelector
  , writeAttributeTemperatureSetpointHoldWithValue_expectedValueInterval_paramsSelector
  , readAttributeTemperatureSetpointHoldDurationWithParamsSelector
  , writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueIntervalSelector
  , writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueInterval_paramsSelector
  , readAttributeThermostatProgrammingOperationModeWithParamsSelector
  , writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueIntervalSelector
  , writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueInterval_paramsSelector
  , readAttributeThermostatRunningStateWithParamsSelector
  , readAttributeSetpointChangeSourceWithParamsSelector
  , readAttributeSetpointChangeAmountWithParamsSelector
  , readAttributeSetpointChangeSourceTimestampWithParamsSelector
  , readAttributeOccupiedSetbackWithParamsSelector
  , writeAttributeOccupiedSetbackWithValue_expectedValueIntervalSelector
  , writeAttributeOccupiedSetbackWithValue_expectedValueInterval_paramsSelector
  , readAttributeOccupiedSetbackMinWithParamsSelector
  , readAttributeOccupiedSetbackMaxWithParamsSelector
  , readAttributeUnoccupiedSetbackWithParamsSelector
  , writeAttributeUnoccupiedSetbackWithValue_expectedValueIntervalSelector
  , writeAttributeUnoccupiedSetbackWithValue_expectedValueInterval_paramsSelector
  , readAttributeUnoccupiedSetbackMinWithParamsSelector
  , readAttributeUnoccupiedSetbackMaxWithParamsSelector
  , readAttributeEmergencyHeatDeltaWithParamsSelector
  , writeAttributeEmergencyHeatDeltaWithValue_expectedValueIntervalSelector
  , writeAttributeEmergencyHeatDeltaWithValue_expectedValueInterval_paramsSelector
  , readAttributeACTypeWithParamsSelector
  , writeAttributeACTypeWithValue_expectedValueIntervalSelector
  , writeAttributeACTypeWithValue_expectedValueInterval_paramsSelector
  , readAttributeACCapacityWithParamsSelector
  , writeAttributeACCapacityWithValue_expectedValueIntervalSelector
  , writeAttributeACCapacityWithValue_expectedValueInterval_paramsSelector
  , readAttributeACRefrigerantTypeWithParamsSelector
  , writeAttributeACRefrigerantTypeWithValue_expectedValueIntervalSelector
  , writeAttributeACRefrigerantTypeWithValue_expectedValueInterval_paramsSelector
  , readAttributeACCompressorTypeWithParamsSelector
  , writeAttributeACCompressorTypeWithValue_expectedValueIntervalSelector
  , writeAttributeACCompressorTypeWithValue_expectedValueInterval_paramsSelector
  , readAttributeACErrorCodeWithParamsSelector
  , writeAttributeACErrorCodeWithValue_expectedValueIntervalSelector
  , writeAttributeACErrorCodeWithValue_expectedValueInterval_paramsSelector
  , readAttributeACLouverPositionWithParamsSelector
  , writeAttributeACLouverPositionWithValue_expectedValueIntervalSelector
  , writeAttributeACLouverPositionWithValue_expectedValueInterval_paramsSelector
  , readAttributeACCoilTemperatureWithParamsSelector
  , readAttributeACCapacityformatWithParamsSelector
  , writeAttributeACCapacityformatWithValue_expectedValueIntervalSelector
  , writeAttributeACCapacityformatWithValue_expectedValueInterval_paramsSelector
  , readAttributePresetTypesWithParamsSelector
  , readAttributeScheduleTypesWithParamsSelector
  , readAttributeNumberOfPresetsWithParamsSelector
  , readAttributeNumberOfSchedulesWithParamsSelector
  , readAttributeNumberOfScheduleTransitionsWithParamsSelector
  , readAttributeNumberOfScheduleTransitionPerDayWithParamsSelector
  , readAttributeActivePresetHandleWithParamsSelector
  , readAttributeActiveScheduleHandleWithParamsSelector
  , readAttributePresetsWithParamsSelector
  , writeAttributePresetsWithValue_expectedValueIntervalSelector
  , writeAttributePresetsWithValue_expectedValueInterval_paramsSelector
  , readAttributeSchedulesWithParamsSelector
  , writeAttributeSchedulesWithValue_expectedValueIntervalSelector
  , writeAttributeSchedulesWithValue_expectedValueInterval_paramsSelector
  , readAttributeSetpointHoldExpiryTimestampWithParamsSelector
  , readAttributeMaxThermostatSuggestionsWithParamsSelector
  , readAttributeThermostatSuggestionsWithParamsSelector
  , readAttributeCurrentThermostatSuggestionWithParamsSelector
  , readAttributeThermostatSuggestionNotFollowingReasonWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- setpointRaiseLowerWithParams:expectedValues:expectedValueInterval:completion:@
setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterSetpointRaiseLowerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completion mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "setpointRaiseLowerWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:@
setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterSetWeeklyScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "setWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:@
getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterGetWeeklyScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "getWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:@
clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterClearWeeklyScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completion mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "clearWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearWeeklyScheduleWithExpectedValues:expectedValueInterval:completion:@
clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completion mtrClusterThermostat  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "clearWeeklyScheduleWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setActiveScheduleRequestWithParams:expectedValues:expectedValueInterval:completion:@
setActiveScheduleRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterSetActiveScheduleRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setActiveScheduleRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "setActiveScheduleRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setActivePresetRequestWithParams:expectedValues:expectedValueInterval:completion:@
setActivePresetRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterSetActivePresetRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setActivePresetRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "setActivePresetRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addThermostatSuggestionWithParams:expectedValues:expectedValueInterval:completion:@
addThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterAddThermostatSuggestionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completion mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "addThermostatSuggestionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeThermostatSuggestionWithParams:expectedValues:expectedValueInterval:completion:@
removeThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterRemoveThermostatSuggestionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completion mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "removeThermostatSuggestionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- atomicRequestWithParams:expectedValues:expectedValueInterval:completion:@
atomicRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterAtomicRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
atomicRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "atomicRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLocalTemperatureWithParams:@
readAttributeLocalTemperatureWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeLocalTemperatureWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeLocalTemperatureWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOutdoorTemperatureWithParams:@
readAttributeOutdoorTemperatureWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeOutdoorTemperatureWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeOutdoorTemperatureWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOccupancyWithParams:@
readAttributeOccupancyWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeOccupancyWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeOccupancyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAbsMinHeatSetpointLimitWithParams:@
readAttributeAbsMinHeatSetpointLimitWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeAbsMinHeatSetpointLimitWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeAbsMinHeatSetpointLimitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAbsMaxHeatSetpointLimitWithParams:@
readAttributeAbsMaxHeatSetpointLimitWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeAbsMaxHeatSetpointLimitWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeAbsMaxHeatSetpointLimitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAbsMinCoolSetpointLimitWithParams:@
readAttributeAbsMinCoolSetpointLimitWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeAbsMinCoolSetpointLimitWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeAbsMinCoolSetpointLimitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAbsMaxCoolSetpointLimitWithParams:@
readAttributeAbsMaxCoolSetpointLimitWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeAbsMaxCoolSetpointLimitWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeAbsMaxCoolSetpointLimitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePICoolingDemandWithParams:@
readAttributePICoolingDemandWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributePICoolingDemandWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributePICoolingDemandWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePIHeatingDemandWithParams:@
readAttributePIHeatingDemandWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributePIHeatingDemandWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributePIHeatingDemandWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeHVACSystemTypeConfigurationWithParams:@
readAttributeHVACSystemTypeConfigurationWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeHVACSystemTypeConfigurationWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeHVACSystemTypeConfigurationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeHVACSystemTypeConfigurationWithValue:expectedValueInterval:@
writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeHVACSystemTypeConfigurationWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeHVACSystemTypeConfigurationWithValue:expectedValueInterval:params:@
writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeHVACSystemTypeConfigurationWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLocalTemperatureCalibrationWithParams:@
readAttributeLocalTemperatureCalibrationWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeLocalTemperatureCalibrationWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeLocalTemperatureCalibrationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLocalTemperatureCalibrationWithValue:expectedValueInterval:@
writeAttributeLocalTemperatureCalibrationWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLocalTemperatureCalibrationWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeLocalTemperatureCalibrationWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLocalTemperatureCalibrationWithValue:expectedValueInterval:params:@
writeAttributeLocalTemperatureCalibrationWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLocalTemperatureCalibrationWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeLocalTemperatureCalibrationWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOccupiedCoolingSetpointWithParams:@
readAttributeOccupiedCoolingSetpointWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeOccupiedCoolingSetpointWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeOccupiedCoolingSetpointWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOccupiedCoolingSetpointWithValue:expectedValueInterval:@
writeAttributeOccupiedCoolingSetpointWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOccupiedCoolingSetpointWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeOccupiedCoolingSetpointWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOccupiedCoolingSetpointWithValue:expectedValueInterval:params:@
writeAttributeOccupiedCoolingSetpointWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOccupiedCoolingSetpointWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeOccupiedCoolingSetpointWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOccupiedHeatingSetpointWithParams:@
readAttributeOccupiedHeatingSetpointWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeOccupiedHeatingSetpointWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeOccupiedHeatingSetpointWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOccupiedHeatingSetpointWithValue:expectedValueInterval:@
writeAttributeOccupiedHeatingSetpointWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOccupiedHeatingSetpointWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeOccupiedHeatingSetpointWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOccupiedHeatingSetpointWithValue:expectedValueInterval:params:@
writeAttributeOccupiedHeatingSetpointWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOccupiedHeatingSetpointWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeOccupiedHeatingSetpointWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeUnoccupiedCoolingSetpointWithParams:@
readAttributeUnoccupiedCoolingSetpointWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeUnoccupiedCoolingSetpointWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeUnoccupiedCoolingSetpointWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeUnoccupiedCoolingSetpointWithValue:expectedValueInterval:@
writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeUnoccupiedCoolingSetpointWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeUnoccupiedCoolingSetpointWithValue:expectedValueInterval:params:@
writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeUnoccupiedCoolingSetpointWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeUnoccupiedHeatingSetpointWithParams:@
readAttributeUnoccupiedHeatingSetpointWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeUnoccupiedHeatingSetpointWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeUnoccupiedHeatingSetpointWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeUnoccupiedHeatingSetpointWithValue:expectedValueInterval:@
writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeUnoccupiedHeatingSetpointWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeUnoccupiedHeatingSetpointWithValue:expectedValueInterval:params:@
writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeUnoccupiedHeatingSetpointWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeMinHeatSetpointLimitWithParams:@
readAttributeMinHeatSetpointLimitWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeMinHeatSetpointLimitWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeMinHeatSetpointLimitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeMinHeatSetpointLimitWithValue:expectedValueInterval:@
writeAttributeMinHeatSetpointLimitWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMinHeatSetpointLimitWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeMinHeatSetpointLimitWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeMinHeatSetpointLimitWithValue:expectedValueInterval:params:@
writeAttributeMinHeatSetpointLimitWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMinHeatSetpointLimitWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeMinHeatSetpointLimitWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeMaxHeatSetpointLimitWithParams:@
readAttributeMaxHeatSetpointLimitWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeMaxHeatSetpointLimitWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeMaxHeatSetpointLimitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeMaxHeatSetpointLimitWithValue:expectedValueInterval:@
writeAttributeMaxHeatSetpointLimitWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMaxHeatSetpointLimitWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeMaxHeatSetpointLimitWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeMaxHeatSetpointLimitWithValue:expectedValueInterval:params:@
writeAttributeMaxHeatSetpointLimitWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMaxHeatSetpointLimitWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeMaxHeatSetpointLimitWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeMinCoolSetpointLimitWithParams:@
readAttributeMinCoolSetpointLimitWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeMinCoolSetpointLimitWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeMinCoolSetpointLimitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeMinCoolSetpointLimitWithValue:expectedValueInterval:@
writeAttributeMinCoolSetpointLimitWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMinCoolSetpointLimitWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeMinCoolSetpointLimitWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeMinCoolSetpointLimitWithValue:expectedValueInterval:params:@
writeAttributeMinCoolSetpointLimitWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMinCoolSetpointLimitWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeMinCoolSetpointLimitWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeMaxCoolSetpointLimitWithParams:@
readAttributeMaxCoolSetpointLimitWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeMaxCoolSetpointLimitWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeMaxCoolSetpointLimitWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeMaxCoolSetpointLimitWithValue:expectedValueInterval:@
writeAttributeMaxCoolSetpointLimitWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMaxCoolSetpointLimitWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeMaxCoolSetpointLimitWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeMaxCoolSetpointLimitWithValue:expectedValueInterval:params:@
writeAttributeMaxCoolSetpointLimitWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMaxCoolSetpointLimitWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeMaxCoolSetpointLimitWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeMinSetpointDeadBandWithParams:@
readAttributeMinSetpointDeadBandWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeMinSetpointDeadBandWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeMinSetpointDeadBandWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeMinSetpointDeadBandWithValue:expectedValueInterval:@
writeAttributeMinSetpointDeadBandWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMinSetpointDeadBandWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeMinSetpointDeadBandWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeMinSetpointDeadBandWithValue:expectedValueInterval:params:@
writeAttributeMinSetpointDeadBandWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMinSetpointDeadBandWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeMinSetpointDeadBandWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeRemoteSensingWithParams:@
readAttributeRemoteSensingWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeRemoteSensingWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeRemoteSensingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeRemoteSensingWithValue:expectedValueInterval:@
writeAttributeRemoteSensingWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRemoteSensingWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeRemoteSensingWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeRemoteSensingWithValue:expectedValueInterval:params:@
writeAttributeRemoteSensingWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRemoteSensingWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeRemoteSensingWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeControlSequenceOfOperationWithParams:@
readAttributeControlSequenceOfOperationWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeControlSequenceOfOperationWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeControlSequenceOfOperationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeControlSequenceOfOperationWithValue:expectedValueInterval:@
writeAttributeControlSequenceOfOperationWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeControlSequenceOfOperationWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeControlSequenceOfOperationWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeControlSequenceOfOperationWithValue:expectedValueInterval:params:@
writeAttributeControlSequenceOfOperationWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeControlSequenceOfOperationWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeControlSequenceOfOperationWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSystemModeWithParams:@
readAttributeSystemModeWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeSystemModeWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeSystemModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSystemModeWithValue:expectedValueInterval:@
writeAttributeSystemModeWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSystemModeWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeSystemModeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSystemModeWithValue:expectedValueInterval:params:@
writeAttributeSystemModeWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSystemModeWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeSystemModeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeThermostatRunningModeWithParams:@
readAttributeThermostatRunningModeWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeThermostatRunningModeWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeThermostatRunningModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStartOfWeekWithParams:@
readAttributeStartOfWeekWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeStartOfWeekWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeStartOfWeekWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfWeeklyTransitionsWithParams:@
readAttributeNumberOfWeeklyTransitionsWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeNumberOfWeeklyTransitionsWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeNumberOfWeeklyTransitionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfDailyTransitionsWithParams:@
readAttributeNumberOfDailyTransitionsWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeNumberOfDailyTransitionsWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeNumberOfDailyTransitionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTemperatureSetpointHoldWithParams:@
readAttributeTemperatureSetpointHoldWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeTemperatureSetpointHoldWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeTemperatureSetpointHoldWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeTemperatureSetpointHoldWithValue:expectedValueInterval:@
writeAttributeTemperatureSetpointHoldWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeTemperatureSetpointHoldWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeTemperatureSetpointHoldWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeTemperatureSetpointHoldWithValue:expectedValueInterval:params:@
writeAttributeTemperatureSetpointHoldWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeTemperatureSetpointHoldWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeTemperatureSetpointHoldWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeTemperatureSetpointHoldDurationWithParams:@
readAttributeTemperatureSetpointHoldDurationWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeTemperatureSetpointHoldDurationWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeTemperatureSetpointHoldDurationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeTemperatureSetpointHoldDurationWithValue:expectedValueInterval:@
writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeTemperatureSetpointHoldDurationWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeTemperatureSetpointHoldDurationWithValue:expectedValueInterval:params:@
writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeTemperatureSetpointHoldDurationWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeThermostatProgrammingOperationModeWithParams:@
readAttributeThermostatProgrammingOperationModeWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeThermostatProgrammingOperationModeWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeThermostatProgrammingOperationModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeThermostatProgrammingOperationModeWithValue:expectedValueInterval:@
writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeThermostatProgrammingOperationModeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeThermostatProgrammingOperationModeWithValue:expectedValueInterval:params:@
writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeThermostatProgrammingOperationModeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeThermostatRunningStateWithParams:@
readAttributeThermostatRunningStateWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeThermostatRunningStateWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeThermostatRunningStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSetpointChangeSourceWithParams:@
readAttributeSetpointChangeSourceWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeSetpointChangeSourceWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeSetpointChangeSourceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSetpointChangeAmountWithParams:@
readAttributeSetpointChangeAmountWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeSetpointChangeAmountWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeSetpointChangeAmountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSetpointChangeSourceTimestampWithParams:@
readAttributeSetpointChangeSourceTimestampWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeSetpointChangeSourceTimestampWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeSetpointChangeSourceTimestampWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOccupiedSetbackWithParams:@
readAttributeOccupiedSetbackWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeOccupiedSetbackWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeOccupiedSetbackWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOccupiedSetbackWithValue:expectedValueInterval:@
writeAttributeOccupiedSetbackWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOccupiedSetbackWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeOccupiedSetbackWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOccupiedSetbackWithValue:expectedValueInterval:params:@
writeAttributeOccupiedSetbackWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOccupiedSetbackWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeOccupiedSetbackWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOccupiedSetbackMinWithParams:@
readAttributeOccupiedSetbackMinWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeOccupiedSetbackMinWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeOccupiedSetbackMinWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOccupiedSetbackMaxWithParams:@
readAttributeOccupiedSetbackMaxWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeOccupiedSetbackMaxWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeOccupiedSetbackMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUnoccupiedSetbackWithParams:@
readAttributeUnoccupiedSetbackWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeUnoccupiedSetbackWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeUnoccupiedSetbackWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeUnoccupiedSetbackWithValue:expectedValueInterval:@
writeAttributeUnoccupiedSetbackWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeUnoccupiedSetbackWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeUnoccupiedSetbackWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeUnoccupiedSetbackWithValue:expectedValueInterval:params:@
writeAttributeUnoccupiedSetbackWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeUnoccupiedSetbackWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeUnoccupiedSetbackWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeUnoccupiedSetbackMinWithParams:@
readAttributeUnoccupiedSetbackMinWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeUnoccupiedSetbackMinWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeUnoccupiedSetbackMinWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUnoccupiedSetbackMaxWithParams:@
readAttributeUnoccupiedSetbackMaxWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeUnoccupiedSetbackMaxWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeUnoccupiedSetbackMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEmergencyHeatDeltaWithParams:@
readAttributeEmergencyHeatDeltaWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeEmergencyHeatDeltaWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeEmergencyHeatDeltaWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeEmergencyHeatDeltaWithValue:expectedValueInterval:@
writeAttributeEmergencyHeatDeltaWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeEmergencyHeatDeltaWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeEmergencyHeatDeltaWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeEmergencyHeatDeltaWithValue:expectedValueInterval:params:@
writeAttributeEmergencyHeatDeltaWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeEmergencyHeatDeltaWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeEmergencyHeatDeltaWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeACTypeWithParams:@
readAttributeACTypeWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeACTypeWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeACTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeACTypeWithValue:expectedValueInterval:@
writeAttributeACTypeWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeACTypeWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeACTypeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeACTypeWithValue:expectedValueInterval:params:@
writeAttributeACTypeWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeACTypeWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeACTypeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeACCapacityWithParams:@
readAttributeACCapacityWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeACCapacityWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeACCapacityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeACCapacityWithValue:expectedValueInterval:@
writeAttributeACCapacityWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeACCapacityWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeACCapacityWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeACCapacityWithValue:expectedValueInterval:params:@
writeAttributeACCapacityWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeACCapacityWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeACCapacityWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeACRefrigerantTypeWithParams:@
readAttributeACRefrigerantTypeWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeACRefrigerantTypeWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeACRefrigerantTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeACRefrigerantTypeWithValue:expectedValueInterval:@
writeAttributeACRefrigerantTypeWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeACRefrigerantTypeWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeACRefrigerantTypeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeACRefrigerantTypeWithValue:expectedValueInterval:params:@
writeAttributeACRefrigerantTypeWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeACRefrigerantTypeWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeACRefrigerantTypeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeACCompressorTypeWithParams:@
readAttributeACCompressorTypeWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeACCompressorTypeWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeACCompressorTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeACCompressorTypeWithValue:expectedValueInterval:@
writeAttributeACCompressorTypeWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeACCompressorTypeWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeACCompressorTypeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeACCompressorTypeWithValue:expectedValueInterval:params:@
writeAttributeACCompressorTypeWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeACCompressorTypeWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeACCompressorTypeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeACErrorCodeWithParams:@
readAttributeACErrorCodeWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeACErrorCodeWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeACErrorCodeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeACErrorCodeWithValue:expectedValueInterval:@
writeAttributeACErrorCodeWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeACErrorCodeWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeACErrorCodeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeACErrorCodeWithValue:expectedValueInterval:params:@
writeAttributeACErrorCodeWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeACErrorCodeWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeACErrorCodeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeACLouverPositionWithParams:@
readAttributeACLouverPositionWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeACLouverPositionWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeACLouverPositionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeACLouverPositionWithValue:expectedValueInterval:@
writeAttributeACLouverPositionWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeACLouverPositionWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeACLouverPositionWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeACLouverPositionWithValue:expectedValueInterval:params:@
writeAttributeACLouverPositionWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeACLouverPositionWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeACLouverPositionWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeACCoilTemperatureWithParams:@
readAttributeACCoilTemperatureWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeACCoilTemperatureWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeACCoilTemperatureWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeACCapacityformatWithParams:@
readAttributeACCapacityformatWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeACCapacityformatWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeACCapacityformatWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeACCapacityformatWithValue:expectedValueInterval:@
writeAttributeACCapacityformatWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeACCapacityformatWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeACCapacityformatWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeACCapacityformatWithValue:expectedValueInterval:params:@
writeAttributeACCapacityformatWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeACCapacityformatWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeACCapacityformatWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePresetTypesWithParams:@
readAttributePresetTypesWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributePresetTypesWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributePresetTypesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeScheduleTypesWithParams:@
readAttributeScheduleTypesWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeScheduleTypesWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeScheduleTypesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfPresetsWithParams:@
readAttributeNumberOfPresetsWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeNumberOfPresetsWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeNumberOfPresetsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfSchedulesWithParams:@
readAttributeNumberOfSchedulesWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeNumberOfSchedulesWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeNumberOfSchedulesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfScheduleTransitionsWithParams:@
readAttributeNumberOfScheduleTransitionsWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeNumberOfScheduleTransitionsWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeNumberOfScheduleTransitionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNumberOfScheduleTransitionPerDayWithParams:@
readAttributeNumberOfScheduleTransitionPerDayWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeNumberOfScheduleTransitionPerDayWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeNumberOfScheduleTransitionPerDayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActivePresetHandleWithParams:@
readAttributeActivePresetHandleWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeActivePresetHandleWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeActivePresetHandleWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveScheduleHandleWithParams:@
readAttributeActiveScheduleHandleWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeActiveScheduleHandleWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeActiveScheduleHandleWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePresetsWithParams:@
readAttributePresetsWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributePresetsWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributePresetsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePresetsWithValue:expectedValueInterval:@
writeAttributePresetsWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePresetsWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributePresetsWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePresetsWithValue:expectedValueInterval:params:@
writeAttributePresetsWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePresetsWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributePresetsWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSchedulesWithParams:@
readAttributeSchedulesWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeSchedulesWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeSchedulesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSchedulesWithValue:expectedValueInterval:@
writeAttributeSchedulesWithValue_expectedValueInterval :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSchedulesWithValue_expectedValueInterval mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "writeAttributeSchedulesWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSchedulesWithValue:expectedValueInterval:params:@
writeAttributeSchedulesWithValue_expectedValueInterval_params :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostat -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSchedulesWithValue_expectedValueInterval_params mtrClusterThermostat  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostat (mkSelector "writeAttributeSchedulesWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSetpointHoldExpiryTimestampWithParams:@
readAttributeSetpointHoldExpiryTimestampWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeSetpointHoldExpiryTimestampWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeSetpointHoldExpiryTimestampWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxThermostatSuggestionsWithParams:@
readAttributeMaxThermostatSuggestionsWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeMaxThermostatSuggestionsWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeMaxThermostatSuggestionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeThermostatSuggestionsWithParams:@
readAttributeThermostatSuggestionsWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeThermostatSuggestionsWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeThermostatSuggestionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentThermostatSuggestionWithParams:@
readAttributeCurrentThermostatSuggestionWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeCurrentThermostatSuggestionWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeCurrentThermostatSuggestionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeThermostatSuggestionNotFollowingReasonWithParams:@
readAttributeThermostatSuggestionNotFollowingReasonWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeThermostatSuggestionNotFollowingReasonWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeThermostatSuggestionNotFollowingReasonWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRReadParams params) => mtrClusterThermostat -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterThermostat  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostat (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterThermostat mtrClusterThermostat => mtrClusterThermostat -> IO (Id MTRClusterThermostat)
init_ mtrClusterThermostat  =
    sendMsg mtrClusterThermostat (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterThermostat)
new  =
  do
    cls' <- getRequiredClass "MTRClusterThermostat"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRDevice device, IsNSObject queue) => mtrClusterThermostat -> device -> CUShort -> queue -> IO (Id MTRClusterThermostat)
initWithDevice_endpoint_queue mtrClusterThermostat  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterThermostat (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- setpointRaiseLowerWithParams:expectedValues:expectedValueInterval:completionHandler:@
setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterSetpointRaiseLowerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "setpointRaiseLowerWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterSetWeeklyScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "setWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- getWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterGetWeeklyScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "getWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- clearWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRThermostatClusterClearWeeklyScheduleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterThermostat  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThermostat (mkSelector "clearWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- clearWeeklyScheduleWithExpectedValues:expectedValueInterval:completionHandler:@
clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterThermostat mtrClusterThermostat, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostat -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completionHandler mtrClusterThermostat  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostat (mkSelector "clearWeeklyScheduleWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterThermostat mtrClusterThermostat, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterThermostat -> device -> endpointID -> queue -> IO (Id MTRClusterThermostat)
initWithDevice_endpointID_queue mtrClusterThermostat  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterThermostat (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setpointRaiseLowerWithParams:expectedValues:expectedValueInterval:completion:@
setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setpointRaiseLowerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:@
setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:@
getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:@
clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "clearWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearWeeklyScheduleWithExpectedValues:expectedValueInterval:completion:@
clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completionSelector :: Selector
clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "clearWeeklyScheduleWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setActiveScheduleRequestWithParams:expectedValues:expectedValueInterval:completion:@
setActiveScheduleRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setActiveScheduleRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setActiveScheduleRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setActivePresetRequestWithParams:expectedValues:expectedValueInterval:completion:@
setActivePresetRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setActivePresetRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setActivePresetRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addThermostatSuggestionWithParams:expectedValues:expectedValueInterval:completion:@
addThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addThermostatSuggestionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeThermostatSuggestionWithParams:expectedValues:expectedValueInterval:completion:@
removeThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeThermostatSuggestionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeThermostatSuggestionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @atomicRequestWithParams:expectedValues:expectedValueInterval:completion:@
atomicRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
atomicRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "atomicRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeLocalTemperatureWithParams:@
readAttributeLocalTemperatureWithParamsSelector :: Selector
readAttributeLocalTemperatureWithParamsSelector = mkSelector "readAttributeLocalTemperatureWithParams:"

-- | @Selector@ for @readAttributeOutdoorTemperatureWithParams:@
readAttributeOutdoorTemperatureWithParamsSelector :: Selector
readAttributeOutdoorTemperatureWithParamsSelector = mkSelector "readAttributeOutdoorTemperatureWithParams:"

-- | @Selector@ for @readAttributeOccupancyWithParams:@
readAttributeOccupancyWithParamsSelector :: Selector
readAttributeOccupancyWithParamsSelector = mkSelector "readAttributeOccupancyWithParams:"

-- | @Selector@ for @readAttributeAbsMinHeatSetpointLimitWithParams:@
readAttributeAbsMinHeatSetpointLimitWithParamsSelector :: Selector
readAttributeAbsMinHeatSetpointLimitWithParamsSelector = mkSelector "readAttributeAbsMinHeatSetpointLimitWithParams:"

-- | @Selector@ for @readAttributeAbsMaxHeatSetpointLimitWithParams:@
readAttributeAbsMaxHeatSetpointLimitWithParamsSelector :: Selector
readAttributeAbsMaxHeatSetpointLimitWithParamsSelector = mkSelector "readAttributeAbsMaxHeatSetpointLimitWithParams:"

-- | @Selector@ for @readAttributeAbsMinCoolSetpointLimitWithParams:@
readAttributeAbsMinCoolSetpointLimitWithParamsSelector :: Selector
readAttributeAbsMinCoolSetpointLimitWithParamsSelector = mkSelector "readAttributeAbsMinCoolSetpointLimitWithParams:"

-- | @Selector@ for @readAttributeAbsMaxCoolSetpointLimitWithParams:@
readAttributeAbsMaxCoolSetpointLimitWithParamsSelector :: Selector
readAttributeAbsMaxCoolSetpointLimitWithParamsSelector = mkSelector "readAttributeAbsMaxCoolSetpointLimitWithParams:"

-- | @Selector@ for @readAttributePICoolingDemandWithParams:@
readAttributePICoolingDemandWithParamsSelector :: Selector
readAttributePICoolingDemandWithParamsSelector = mkSelector "readAttributePICoolingDemandWithParams:"

-- | @Selector@ for @readAttributePIHeatingDemandWithParams:@
readAttributePIHeatingDemandWithParamsSelector :: Selector
readAttributePIHeatingDemandWithParamsSelector = mkSelector "readAttributePIHeatingDemandWithParams:"

-- | @Selector@ for @readAttributeHVACSystemTypeConfigurationWithParams:@
readAttributeHVACSystemTypeConfigurationWithParamsSelector :: Selector
readAttributeHVACSystemTypeConfigurationWithParamsSelector = mkSelector "readAttributeHVACSystemTypeConfigurationWithParams:"

-- | @Selector@ for @writeAttributeHVACSystemTypeConfigurationWithValue:expectedValueInterval:@
writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueIntervalSelector :: Selector
writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeHVACSystemTypeConfigurationWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeHVACSystemTypeConfigurationWithValue:expectedValueInterval:params:@
writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeHVACSystemTypeConfigurationWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeHVACSystemTypeConfigurationWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLocalTemperatureCalibrationWithParams:@
readAttributeLocalTemperatureCalibrationWithParamsSelector :: Selector
readAttributeLocalTemperatureCalibrationWithParamsSelector = mkSelector "readAttributeLocalTemperatureCalibrationWithParams:"

-- | @Selector@ for @writeAttributeLocalTemperatureCalibrationWithValue:expectedValueInterval:@
writeAttributeLocalTemperatureCalibrationWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLocalTemperatureCalibrationWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLocalTemperatureCalibrationWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLocalTemperatureCalibrationWithValue:expectedValueInterval:params:@
writeAttributeLocalTemperatureCalibrationWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLocalTemperatureCalibrationWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLocalTemperatureCalibrationWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOccupiedCoolingSetpointWithParams:@
readAttributeOccupiedCoolingSetpointWithParamsSelector :: Selector
readAttributeOccupiedCoolingSetpointWithParamsSelector = mkSelector "readAttributeOccupiedCoolingSetpointWithParams:"

-- | @Selector@ for @writeAttributeOccupiedCoolingSetpointWithValue:expectedValueInterval:@
writeAttributeOccupiedCoolingSetpointWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOccupiedCoolingSetpointWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOccupiedCoolingSetpointWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOccupiedCoolingSetpointWithValue:expectedValueInterval:params:@
writeAttributeOccupiedCoolingSetpointWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOccupiedCoolingSetpointWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOccupiedCoolingSetpointWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOccupiedHeatingSetpointWithParams:@
readAttributeOccupiedHeatingSetpointWithParamsSelector :: Selector
readAttributeOccupiedHeatingSetpointWithParamsSelector = mkSelector "readAttributeOccupiedHeatingSetpointWithParams:"

-- | @Selector@ for @writeAttributeOccupiedHeatingSetpointWithValue:expectedValueInterval:@
writeAttributeOccupiedHeatingSetpointWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOccupiedHeatingSetpointWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOccupiedHeatingSetpointWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOccupiedHeatingSetpointWithValue:expectedValueInterval:params:@
writeAttributeOccupiedHeatingSetpointWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOccupiedHeatingSetpointWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOccupiedHeatingSetpointWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeUnoccupiedCoolingSetpointWithParams:@
readAttributeUnoccupiedCoolingSetpointWithParamsSelector :: Selector
readAttributeUnoccupiedCoolingSetpointWithParamsSelector = mkSelector "readAttributeUnoccupiedCoolingSetpointWithParams:"

-- | @Selector@ for @writeAttributeUnoccupiedCoolingSetpointWithValue:expectedValueInterval:@
writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueIntervalSelector :: Selector
writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeUnoccupiedCoolingSetpointWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeUnoccupiedCoolingSetpointWithValue:expectedValueInterval:params:@
writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeUnoccupiedCoolingSetpointWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeUnoccupiedCoolingSetpointWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeUnoccupiedHeatingSetpointWithParams:@
readAttributeUnoccupiedHeatingSetpointWithParamsSelector :: Selector
readAttributeUnoccupiedHeatingSetpointWithParamsSelector = mkSelector "readAttributeUnoccupiedHeatingSetpointWithParams:"

-- | @Selector@ for @writeAttributeUnoccupiedHeatingSetpointWithValue:expectedValueInterval:@
writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueIntervalSelector :: Selector
writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeUnoccupiedHeatingSetpointWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeUnoccupiedHeatingSetpointWithValue:expectedValueInterval:params:@
writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeUnoccupiedHeatingSetpointWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeUnoccupiedHeatingSetpointWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeMinHeatSetpointLimitWithParams:@
readAttributeMinHeatSetpointLimitWithParamsSelector :: Selector
readAttributeMinHeatSetpointLimitWithParamsSelector = mkSelector "readAttributeMinHeatSetpointLimitWithParams:"

-- | @Selector@ for @writeAttributeMinHeatSetpointLimitWithValue:expectedValueInterval:@
writeAttributeMinHeatSetpointLimitWithValue_expectedValueIntervalSelector :: Selector
writeAttributeMinHeatSetpointLimitWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMinHeatSetpointLimitWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMinHeatSetpointLimitWithValue:expectedValueInterval:params:@
writeAttributeMinHeatSetpointLimitWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeMinHeatSetpointLimitWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMinHeatSetpointLimitWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeMaxHeatSetpointLimitWithParams:@
readAttributeMaxHeatSetpointLimitWithParamsSelector :: Selector
readAttributeMaxHeatSetpointLimitWithParamsSelector = mkSelector "readAttributeMaxHeatSetpointLimitWithParams:"

-- | @Selector@ for @writeAttributeMaxHeatSetpointLimitWithValue:expectedValueInterval:@
writeAttributeMaxHeatSetpointLimitWithValue_expectedValueIntervalSelector :: Selector
writeAttributeMaxHeatSetpointLimitWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMaxHeatSetpointLimitWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMaxHeatSetpointLimitWithValue:expectedValueInterval:params:@
writeAttributeMaxHeatSetpointLimitWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeMaxHeatSetpointLimitWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMaxHeatSetpointLimitWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeMinCoolSetpointLimitWithParams:@
readAttributeMinCoolSetpointLimitWithParamsSelector :: Selector
readAttributeMinCoolSetpointLimitWithParamsSelector = mkSelector "readAttributeMinCoolSetpointLimitWithParams:"

-- | @Selector@ for @writeAttributeMinCoolSetpointLimitWithValue:expectedValueInterval:@
writeAttributeMinCoolSetpointLimitWithValue_expectedValueIntervalSelector :: Selector
writeAttributeMinCoolSetpointLimitWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMinCoolSetpointLimitWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMinCoolSetpointLimitWithValue:expectedValueInterval:params:@
writeAttributeMinCoolSetpointLimitWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeMinCoolSetpointLimitWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMinCoolSetpointLimitWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeMaxCoolSetpointLimitWithParams:@
readAttributeMaxCoolSetpointLimitWithParamsSelector :: Selector
readAttributeMaxCoolSetpointLimitWithParamsSelector = mkSelector "readAttributeMaxCoolSetpointLimitWithParams:"

-- | @Selector@ for @writeAttributeMaxCoolSetpointLimitWithValue:expectedValueInterval:@
writeAttributeMaxCoolSetpointLimitWithValue_expectedValueIntervalSelector :: Selector
writeAttributeMaxCoolSetpointLimitWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMaxCoolSetpointLimitWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMaxCoolSetpointLimitWithValue:expectedValueInterval:params:@
writeAttributeMaxCoolSetpointLimitWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeMaxCoolSetpointLimitWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMaxCoolSetpointLimitWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeMinSetpointDeadBandWithParams:@
readAttributeMinSetpointDeadBandWithParamsSelector :: Selector
readAttributeMinSetpointDeadBandWithParamsSelector = mkSelector "readAttributeMinSetpointDeadBandWithParams:"

-- | @Selector@ for @writeAttributeMinSetpointDeadBandWithValue:expectedValueInterval:@
writeAttributeMinSetpointDeadBandWithValue_expectedValueIntervalSelector :: Selector
writeAttributeMinSetpointDeadBandWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMinSetpointDeadBandWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMinSetpointDeadBandWithValue:expectedValueInterval:params:@
writeAttributeMinSetpointDeadBandWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeMinSetpointDeadBandWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMinSetpointDeadBandWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRemoteSensingWithParams:@
readAttributeRemoteSensingWithParamsSelector :: Selector
readAttributeRemoteSensingWithParamsSelector = mkSelector "readAttributeRemoteSensingWithParams:"

-- | @Selector@ for @writeAttributeRemoteSensingWithValue:expectedValueInterval:@
writeAttributeRemoteSensingWithValue_expectedValueIntervalSelector :: Selector
writeAttributeRemoteSensingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRemoteSensingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRemoteSensingWithValue:expectedValueInterval:params:@
writeAttributeRemoteSensingWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeRemoteSensingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRemoteSensingWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeControlSequenceOfOperationWithParams:@
readAttributeControlSequenceOfOperationWithParamsSelector :: Selector
readAttributeControlSequenceOfOperationWithParamsSelector = mkSelector "readAttributeControlSequenceOfOperationWithParams:"

-- | @Selector@ for @writeAttributeControlSequenceOfOperationWithValue:expectedValueInterval:@
writeAttributeControlSequenceOfOperationWithValue_expectedValueIntervalSelector :: Selector
writeAttributeControlSequenceOfOperationWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeControlSequenceOfOperationWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeControlSequenceOfOperationWithValue:expectedValueInterval:params:@
writeAttributeControlSequenceOfOperationWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeControlSequenceOfOperationWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeControlSequenceOfOperationWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSystemModeWithParams:@
readAttributeSystemModeWithParamsSelector :: Selector
readAttributeSystemModeWithParamsSelector = mkSelector "readAttributeSystemModeWithParams:"

-- | @Selector@ for @writeAttributeSystemModeWithValue:expectedValueInterval:@
writeAttributeSystemModeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSystemModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSystemModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSystemModeWithValue:expectedValueInterval:params:@
writeAttributeSystemModeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSystemModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSystemModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeThermostatRunningModeWithParams:@
readAttributeThermostatRunningModeWithParamsSelector :: Selector
readAttributeThermostatRunningModeWithParamsSelector = mkSelector "readAttributeThermostatRunningModeWithParams:"

-- | @Selector@ for @readAttributeStartOfWeekWithParams:@
readAttributeStartOfWeekWithParamsSelector :: Selector
readAttributeStartOfWeekWithParamsSelector = mkSelector "readAttributeStartOfWeekWithParams:"

-- | @Selector@ for @readAttributeNumberOfWeeklyTransitionsWithParams:@
readAttributeNumberOfWeeklyTransitionsWithParamsSelector :: Selector
readAttributeNumberOfWeeklyTransitionsWithParamsSelector = mkSelector "readAttributeNumberOfWeeklyTransitionsWithParams:"

-- | @Selector@ for @readAttributeNumberOfDailyTransitionsWithParams:@
readAttributeNumberOfDailyTransitionsWithParamsSelector :: Selector
readAttributeNumberOfDailyTransitionsWithParamsSelector = mkSelector "readAttributeNumberOfDailyTransitionsWithParams:"

-- | @Selector@ for @readAttributeTemperatureSetpointHoldWithParams:@
readAttributeTemperatureSetpointHoldWithParamsSelector :: Selector
readAttributeTemperatureSetpointHoldWithParamsSelector = mkSelector "readAttributeTemperatureSetpointHoldWithParams:"

-- | @Selector@ for @writeAttributeTemperatureSetpointHoldWithValue:expectedValueInterval:@
writeAttributeTemperatureSetpointHoldWithValue_expectedValueIntervalSelector :: Selector
writeAttributeTemperatureSetpointHoldWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeTemperatureSetpointHoldWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeTemperatureSetpointHoldWithValue:expectedValueInterval:params:@
writeAttributeTemperatureSetpointHoldWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeTemperatureSetpointHoldWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeTemperatureSetpointHoldWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeTemperatureSetpointHoldDurationWithParams:@
readAttributeTemperatureSetpointHoldDurationWithParamsSelector :: Selector
readAttributeTemperatureSetpointHoldDurationWithParamsSelector = mkSelector "readAttributeTemperatureSetpointHoldDurationWithParams:"

-- | @Selector@ for @writeAttributeTemperatureSetpointHoldDurationWithValue:expectedValueInterval:@
writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueIntervalSelector :: Selector
writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeTemperatureSetpointHoldDurationWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeTemperatureSetpointHoldDurationWithValue:expectedValueInterval:params:@
writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeTemperatureSetpointHoldDurationWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeTemperatureSetpointHoldDurationWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeThermostatProgrammingOperationModeWithParams:@
readAttributeThermostatProgrammingOperationModeWithParamsSelector :: Selector
readAttributeThermostatProgrammingOperationModeWithParamsSelector = mkSelector "readAttributeThermostatProgrammingOperationModeWithParams:"

-- | @Selector@ for @writeAttributeThermostatProgrammingOperationModeWithValue:expectedValueInterval:@
writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeThermostatProgrammingOperationModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeThermostatProgrammingOperationModeWithValue:expectedValueInterval:params:@
writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeThermostatProgrammingOperationModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeThermostatProgrammingOperationModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeThermostatRunningStateWithParams:@
readAttributeThermostatRunningStateWithParamsSelector :: Selector
readAttributeThermostatRunningStateWithParamsSelector = mkSelector "readAttributeThermostatRunningStateWithParams:"

-- | @Selector@ for @readAttributeSetpointChangeSourceWithParams:@
readAttributeSetpointChangeSourceWithParamsSelector :: Selector
readAttributeSetpointChangeSourceWithParamsSelector = mkSelector "readAttributeSetpointChangeSourceWithParams:"

-- | @Selector@ for @readAttributeSetpointChangeAmountWithParams:@
readAttributeSetpointChangeAmountWithParamsSelector :: Selector
readAttributeSetpointChangeAmountWithParamsSelector = mkSelector "readAttributeSetpointChangeAmountWithParams:"

-- | @Selector@ for @readAttributeSetpointChangeSourceTimestampWithParams:@
readAttributeSetpointChangeSourceTimestampWithParamsSelector :: Selector
readAttributeSetpointChangeSourceTimestampWithParamsSelector = mkSelector "readAttributeSetpointChangeSourceTimestampWithParams:"

-- | @Selector@ for @readAttributeOccupiedSetbackWithParams:@
readAttributeOccupiedSetbackWithParamsSelector :: Selector
readAttributeOccupiedSetbackWithParamsSelector = mkSelector "readAttributeOccupiedSetbackWithParams:"

-- | @Selector@ for @writeAttributeOccupiedSetbackWithValue:expectedValueInterval:@
writeAttributeOccupiedSetbackWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOccupiedSetbackWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOccupiedSetbackWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOccupiedSetbackWithValue:expectedValueInterval:params:@
writeAttributeOccupiedSetbackWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOccupiedSetbackWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOccupiedSetbackWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOccupiedSetbackMinWithParams:@
readAttributeOccupiedSetbackMinWithParamsSelector :: Selector
readAttributeOccupiedSetbackMinWithParamsSelector = mkSelector "readAttributeOccupiedSetbackMinWithParams:"

-- | @Selector@ for @readAttributeOccupiedSetbackMaxWithParams:@
readAttributeOccupiedSetbackMaxWithParamsSelector :: Selector
readAttributeOccupiedSetbackMaxWithParamsSelector = mkSelector "readAttributeOccupiedSetbackMaxWithParams:"

-- | @Selector@ for @readAttributeUnoccupiedSetbackWithParams:@
readAttributeUnoccupiedSetbackWithParamsSelector :: Selector
readAttributeUnoccupiedSetbackWithParamsSelector = mkSelector "readAttributeUnoccupiedSetbackWithParams:"

-- | @Selector@ for @writeAttributeUnoccupiedSetbackWithValue:expectedValueInterval:@
writeAttributeUnoccupiedSetbackWithValue_expectedValueIntervalSelector :: Selector
writeAttributeUnoccupiedSetbackWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeUnoccupiedSetbackWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeUnoccupiedSetbackWithValue:expectedValueInterval:params:@
writeAttributeUnoccupiedSetbackWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeUnoccupiedSetbackWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeUnoccupiedSetbackWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeUnoccupiedSetbackMinWithParams:@
readAttributeUnoccupiedSetbackMinWithParamsSelector :: Selector
readAttributeUnoccupiedSetbackMinWithParamsSelector = mkSelector "readAttributeUnoccupiedSetbackMinWithParams:"

-- | @Selector@ for @readAttributeUnoccupiedSetbackMaxWithParams:@
readAttributeUnoccupiedSetbackMaxWithParamsSelector :: Selector
readAttributeUnoccupiedSetbackMaxWithParamsSelector = mkSelector "readAttributeUnoccupiedSetbackMaxWithParams:"

-- | @Selector@ for @readAttributeEmergencyHeatDeltaWithParams:@
readAttributeEmergencyHeatDeltaWithParamsSelector :: Selector
readAttributeEmergencyHeatDeltaWithParamsSelector = mkSelector "readAttributeEmergencyHeatDeltaWithParams:"

-- | @Selector@ for @writeAttributeEmergencyHeatDeltaWithValue:expectedValueInterval:@
writeAttributeEmergencyHeatDeltaWithValue_expectedValueIntervalSelector :: Selector
writeAttributeEmergencyHeatDeltaWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeEmergencyHeatDeltaWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeEmergencyHeatDeltaWithValue:expectedValueInterval:params:@
writeAttributeEmergencyHeatDeltaWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeEmergencyHeatDeltaWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeEmergencyHeatDeltaWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeACTypeWithParams:@
readAttributeACTypeWithParamsSelector :: Selector
readAttributeACTypeWithParamsSelector = mkSelector "readAttributeACTypeWithParams:"

-- | @Selector@ for @writeAttributeACTypeWithValue:expectedValueInterval:@
writeAttributeACTypeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeACTypeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeACTypeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeACTypeWithValue:expectedValueInterval:params:@
writeAttributeACTypeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeACTypeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeACTypeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeACCapacityWithParams:@
readAttributeACCapacityWithParamsSelector :: Selector
readAttributeACCapacityWithParamsSelector = mkSelector "readAttributeACCapacityWithParams:"

-- | @Selector@ for @writeAttributeACCapacityWithValue:expectedValueInterval:@
writeAttributeACCapacityWithValue_expectedValueIntervalSelector :: Selector
writeAttributeACCapacityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeACCapacityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeACCapacityWithValue:expectedValueInterval:params:@
writeAttributeACCapacityWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeACCapacityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeACCapacityWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeACRefrigerantTypeWithParams:@
readAttributeACRefrigerantTypeWithParamsSelector :: Selector
readAttributeACRefrigerantTypeWithParamsSelector = mkSelector "readAttributeACRefrigerantTypeWithParams:"

-- | @Selector@ for @writeAttributeACRefrigerantTypeWithValue:expectedValueInterval:@
writeAttributeACRefrigerantTypeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeACRefrigerantTypeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeACRefrigerantTypeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeACRefrigerantTypeWithValue:expectedValueInterval:params:@
writeAttributeACRefrigerantTypeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeACRefrigerantTypeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeACRefrigerantTypeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeACCompressorTypeWithParams:@
readAttributeACCompressorTypeWithParamsSelector :: Selector
readAttributeACCompressorTypeWithParamsSelector = mkSelector "readAttributeACCompressorTypeWithParams:"

-- | @Selector@ for @writeAttributeACCompressorTypeWithValue:expectedValueInterval:@
writeAttributeACCompressorTypeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeACCompressorTypeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeACCompressorTypeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeACCompressorTypeWithValue:expectedValueInterval:params:@
writeAttributeACCompressorTypeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeACCompressorTypeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeACCompressorTypeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeACErrorCodeWithParams:@
readAttributeACErrorCodeWithParamsSelector :: Selector
readAttributeACErrorCodeWithParamsSelector = mkSelector "readAttributeACErrorCodeWithParams:"

-- | @Selector@ for @writeAttributeACErrorCodeWithValue:expectedValueInterval:@
writeAttributeACErrorCodeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeACErrorCodeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeACErrorCodeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeACErrorCodeWithValue:expectedValueInterval:params:@
writeAttributeACErrorCodeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeACErrorCodeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeACErrorCodeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeACLouverPositionWithParams:@
readAttributeACLouverPositionWithParamsSelector :: Selector
readAttributeACLouverPositionWithParamsSelector = mkSelector "readAttributeACLouverPositionWithParams:"

-- | @Selector@ for @writeAttributeACLouverPositionWithValue:expectedValueInterval:@
writeAttributeACLouverPositionWithValue_expectedValueIntervalSelector :: Selector
writeAttributeACLouverPositionWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeACLouverPositionWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeACLouverPositionWithValue:expectedValueInterval:params:@
writeAttributeACLouverPositionWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeACLouverPositionWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeACLouverPositionWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeACCoilTemperatureWithParams:@
readAttributeACCoilTemperatureWithParamsSelector :: Selector
readAttributeACCoilTemperatureWithParamsSelector = mkSelector "readAttributeACCoilTemperatureWithParams:"

-- | @Selector@ for @readAttributeACCapacityformatWithParams:@
readAttributeACCapacityformatWithParamsSelector :: Selector
readAttributeACCapacityformatWithParamsSelector = mkSelector "readAttributeACCapacityformatWithParams:"

-- | @Selector@ for @writeAttributeACCapacityformatWithValue:expectedValueInterval:@
writeAttributeACCapacityformatWithValue_expectedValueIntervalSelector :: Selector
writeAttributeACCapacityformatWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeACCapacityformatWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeACCapacityformatWithValue:expectedValueInterval:params:@
writeAttributeACCapacityformatWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeACCapacityformatWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeACCapacityformatWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePresetTypesWithParams:@
readAttributePresetTypesWithParamsSelector :: Selector
readAttributePresetTypesWithParamsSelector = mkSelector "readAttributePresetTypesWithParams:"

-- | @Selector@ for @readAttributeScheduleTypesWithParams:@
readAttributeScheduleTypesWithParamsSelector :: Selector
readAttributeScheduleTypesWithParamsSelector = mkSelector "readAttributeScheduleTypesWithParams:"

-- | @Selector@ for @readAttributeNumberOfPresetsWithParams:@
readAttributeNumberOfPresetsWithParamsSelector :: Selector
readAttributeNumberOfPresetsWithParamsSelector = mkSelector "readAttributeNumberOfPresetsWithParams:"

-- | @Selector@ for @readAttributeNumberOfSchedulesWithParams:@
readAttributeNumberOfSchedulesWithParamsSelector :: Selector
readAttributeNumberOfSchedulesWithParamsSelector = mkSelector "readAttributeNumberOfSchedulesWithParams:"

-- | @Selector@ for @readAttributeNumberOfScheduleTransitionsWithParams:@
readAttributeNumberOfScheduleTransitionsWithParamsSelector :: Selector
readAttributeNumberOfScheduleTransitionsWithParamsSelector = mkSelector "readAttributeNumberOfScheduleTransitionsWithParams:"

-- | @Selector@ for @readAttributeNumberOfScheduleTransitionPerDayWithParams:@
readAttributeNumberOfScheduleTransitionPerDayWithParamsSelector :: Selector
readAttributeNumberOfScheduleTransitionPerDayWithParamsSelector = mkSelector "readAttributeNumberOfScheduleTransitionPerDayWithParams:"

-- | @Selector@ for @readAttributeActivePresetHandleWithParams:@
readAttributeActivePresetHandleWithParamsSelector :: Selector
readAttributeActivePresetHandleWithParamsSelector = mkSelector "readAttributeActivePresetHandleWithParams:"

-- | @Selector@ for @readAttributeActiveScheduleHandleWithParams:@
readAttributeActiveScheduleHandleWithParamsSelector :: Selector
readAttributeActiveScheduleHandleWithParamsSelector = mkSelector "readAttributeActiveScheduleHandleWithParams:"

-- | @Selector@ for @readAttributePresetsWithParams:@
readAttributePresetsWithParamsSelector :: Selector
readAttributePresetsWithParamsSelector = mkSelector "readAttributePresetsWithParams:"

-- | @Selector@ for @writeAttributePresetsWithValue:expectedValueInterval:@
writeAttributePresetsWithValue_expectedValueIntervalSelector :: Selector
writeAttributePresetsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePresetsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePresetsWithValue:expectedValueInterval:params:@
writeAttributePresetsWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePresetsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePresetsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSchedulesWithParams:@
readAttributeSchedulesWithParamsSelector :: Selector
readAttributeSchedulesWithParamsSelector = mkSelector "readAttributeSchedulesWithParams:"

-- | @Selector@ for @writeAttributeSchedulesWithValue:expectedValueInterval:@
writeAttributeSchedulesWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSchedulesWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSchedulesWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSchedulesWithValue:expectedValueInterval:params:@
writeAttributeSchedulesWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSchedulesWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSchedulesWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSetpointHoldExpiryTimestampWithParams:@
readAttributeSetpointHoldExpiryTimestampWithParamsSelector :: Selector
readAttributeSetpointHoldExpiryTimestampWithParamsSelector = mkSelector "readAttributeSetpointHoldExpiryTimestampWithParams:"

-- | @Selector@ for @readAttributeMaxThermostatSuggestionsWithParams:@
readAttributeMaxThermostatSuggestionsWithParamsSelector :: Selector
readAttributeMaxThermostatSuggestionsWithParamsSelector = mkSelector "readAttributeMaxThermostatSuggestionsWithParams:"

-- | @Selector@ for @readAttributeThermostatSuggestionsWithParams:@
readAttributeThermostatSuggestionsWithParamsSelector :: Selector
readAttributeThermostatSuggestionsWithParamsSelector = mkSelector "readAttributeThermostatSuggestionsWithParams:"

-- | @Selector@ for @readAttributeCurrentThermostatSuggestionWithParams:@
readAttributeCurrentThermostatSuggestionWithParamsSelector :: Selector
readAttributeCurrentThermostatSuggestionWithParamsSelector = mkSelector "readAttributeCurrentThermostatSuggestionWithParams:"

-- | @Selector@ for @readAttributeThermostatSuggestionNotFollowingReasonWithParams:@
readAttributeThermostatSuggestionNotFollowingReasonWithParamsSelector :: Selector
readAttributeThermostatSuggestionNotFollowingReasonWithParamsSelector = mkSelector "readAttributeThermostatSuggestionNotFollowingReasonWithParams:"

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

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @setpointRaiseLowerWithParams:expectedValues:expectedValueInterval:completionHandler:@
setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
setpointRaiseLowerWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "setpointRaiseLowerWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @setWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
setWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "setWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @getWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
getWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @clearWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:@
clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
clearWeeklyScheduleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "clearWeeklyScheduleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @clearWeeklyScheduleWithExpectedValues:expectedValueInterval:completionHandler:@
clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
clearWeeklyScheduleWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "clearWeeklyScheduleWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

