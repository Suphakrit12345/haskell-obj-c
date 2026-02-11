{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Pump Configuration and Control    An interface for configuring and controlling pumps.
--
-- Generated bindings for @MTRClusterPumpConfigurationAndControl@.
module ObjC.Matter.MTRClusterPumpConfigurationAndControl
  ( MTRClusterPumpConfigurationAndControl
  , IsMTRClusterPumpConfigurationAndControl(..)
  , readAttributeMaxPressureWithParams
  , readAttributeMaxSpeedWithParams
  , readAttributeMaxFlowWithParams
  , readAttributeMinConstPressureWithParams
  , readAttributeMaxConstPressureWithParams
  , readAttributeMinCompPressureWithParams
  , readAttributeMaxCompPressureWithParams
  , readAttributeMinConstSpeedWithParams
  , readAttributeMaxConstSpeedWithParams
  , readAttributeMinConstFlowWithParams
  , readAttributeMaxConstFlowWithParams
  , readAttributeMinConstTempWithParams
  , readAttributeMaxConstTempWithParams
  , readAttributePumpStatusWithParams
  , readAttributeEffectiveOperationModeWithParams
  , readAttributeEffectiveControlModeWithParams
  , readAttributeCapacityWithParams
  , readAttributeSpeedWithParams
  , readAttributeLifetimeRunningHoursWithParams
  , writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval
  , writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_params
  , readAttributePowerWithParams
  , readAttributeLifetimeEnergyConsumedWithParams
  , writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval
  , writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_params
  , readAttributeOperationModeWithParams
  , writeAttributeOperationModeWithValue_expectedValueInterval
  , writeAttributeOperationModeWithValue_expectedValueInterval_params
  , readAttributeControlModeWithParams
  , writeAttributeControlModeWithValue_expectedValueInterval
  , writeAttributeControlModeWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeMaxPressureWithParamsSelector
  , readAttributeMaxSpeedWithParamsSelector
  , readAttributeMaxFlowWithParamsSelector
  , readAttributeMinConstPressureWithParamsSelector
  , readAttributeMaxConstPressureWithParamsSelector
  , readAttributeMinCompPressureWithParamsSelector
  , readAttributeMaxCompPressureWithParamsSelector
  , readAttributeMinConstSpeedWithParamsSelector
  , readAttributeMaxConstSpeedWithParamsSelector
  , readAttributeMinConstFlowWithParamsSelector
  , readAttributeMaxConstFlowWithParamsSelector
  , readAttributeMinConstTempWithParamsSelector
  , readAttributeMaxConstTempWithParamsSelector
  , readAttributePumpStatusWithParamsSelector
  , readAttributeEffectiveOperationModeWithParamsSelector
  , readAttributeEffectiveControlModeWithParamsSelector
  , readAttributeCapacityWithParamsSelector
  , readAttributeSpeedWithParamsSelector
  , readAttributeLifetimeRunningHoursWithParamsSelector
  , writeAttributeLifetimeRunningHoursWithValue_expectedValueIntervalSelector
  , writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_paramsSelector
  , readAttributePowerWithParamsSelector
  , readAttributeLifetimeEnergyConsumedWithParamsSelector
  , writeAttributeLifetimeEnergyConsumedWithValue_expectedValueIntervalSelector
  , writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_paramsSelector
  , readAttributeOperationModeWithParamsSelector
  , writeAttributeOperationModeWithValue_expectedValueIntervalSelector
  , writeAttributeOperationModeWithValue_expectedValueInterval_paramsSelector
  , readAttributeControlModeWithParamsSelector
  , writeAttributeControlModeWithValue_expectedValueIntervalSelector
  , writeAttributeControlModeWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
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

-- | @- readAttributeMaxPressureWithParams:@
readAttributeMaxPressureWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxPressureWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMaxPressureWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxSpeedWithParams:@
readAttributeMaxSpeedWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxSpeedWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMaxSpeedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxFlowWithParams:@
readAttributeMaxFlowWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxFlowWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMaxFlowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinConstPressureWithParams:@
readAttributeMinConstPressureWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMinConstPressureWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMinConstPressureWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxConstPressureWithParams:@
readAttributeMaxConstPressureWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxConstPressureWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMaxConstPressureWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinCompPressureWithParams:@
readAttributeMinCompPressureWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMinCompPressureWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMinCompPressureWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxCompPressureWithParams:@
readAttributeMaxCompPressureWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxCompPressureWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMaxCompPressureWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinConstSpeedWithParams:@
readAttributeMinConstSpeedWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMinConstSpeedWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMinConstSpeedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxConstSpeedWithParams:@
readAttributeMaxConstSpeedWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxConstSpeedWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMaxConstSpeedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinConstFlowWithParams:@
readAttributeMinConstFlowWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMinConstFlowWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMinConstFlowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxConstFlowWithParams:@
readAttributeMaxConstFlowWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxConstFlowWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMaxConstFlowWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinConstTempWithParams:@
readAttributeMinConstTempWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMinConstTempWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMinConstTempWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxConstTempWithParams:@
readAttributeMaxConstTempWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeMaxConstTempWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeMaxConstTempWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePumpStatusWithParams:@
readAttributePumpStatusWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributePumpStatusWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributePumpStatusWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEffectiveOperationModeWithParams:@
readAttributeEffectiveOperationModeWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeEffectiveOperationModeWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeEffectiveOperationModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEffectiveControlModeWithParams:@
readAttributeEffectiveControlModeWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeEffectiveControlModeWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeEffectiveControlModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCapacityWithParams:@
readAttributeCapacityWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeCapacityWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeCapacityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSpeedWithParams:@
readAttributeSpeedWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeSpeedWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeSpeedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLifetimeRunningHoursWithParams:@
readAttributeLifetimeRunningHoursWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeLifetimeRunningHoursWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeLifetimeRunningHoursWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:@
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval mtrClusterPumpConfigurationAndControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:params:@
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_params :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_params mtrClusterPumpConfigurationAndControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributePowerWithParams:@
readAttributePowerWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributePowerWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributePowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLifetimeEnergyConsumedWithParams:@
readAttributeLifetimeEnergyConsumedWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeLifetimeEnergyConsumedWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeLifetimeEnergyConsumedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:@
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval mtrClusterPumpConfigurationAndControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:params:@
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_params :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_params mtrClusterPumpConfigurationAndControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOperationModeWithParams:@
readAttributeOperationModeWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeOperationModeWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeOperationModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOperationModeWithValue:expectedValueInterval:@
writeAttributeOperationModeWithValue_expectedValueInterval :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOperationModeWithValue_expectedValueInterval mtrClusterPumpConfigurationAndControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "writeAttributeOperationModeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOperationModeWithValue:expectedValueInterval:params:@
writeAttributeOperationModeWithValue_expectedValueInterval_params :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOperationModeWithValue_expectedValueInterval_params mtrClusterPumpConfigurationAndControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "writeAttributeOperationModeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeControlModeWithParams:@
readAttributeControlModeWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeControlModeWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeControlModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeControlModeWithValue:expectedValueInterval:@
writeAttributeControlModeWithValue_expectedValueInterval :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeControlModeWithValue_expectedValueInterval mtrClusterPumpConfigurationAndControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "writeAttributeControlModeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeControlModeWithValue:expectedValueInterval:params:@
writeAttributeControlModeWithValue_expectedValueInterval_params :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterPumpConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeControlModeWithValue_expectedValueInterval_params mtrClusterPumpConfigurationAndControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "writeAttributeControlModeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRReadParams params) => mtrClusterPumpConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPumpConfigurationAndControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl => mtrClusterPumpConfigurationAndControl -> IO (Id MTRClusterPumpConfigurationAndControl)
init_ mtrClusterPumpConfigurationAndControl  =
    sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterPumpConfigurationAndControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPumpConfigurationAndControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRDevice device, IsNSObject queue) => mtrClusterPumpConfigurationAndControl -> device -> CUShort -> queue -> IO (Id MTRClusterPumpConfigurationAndControl)
initWithDevice_endpoint_queue mtrClusterPumpConfigurationAndControl  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPumpConfigurationAndControl mtrClusterPumpConfigurationAndControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPumpConfigurationAndControl -> device -> endpointID -> queue -> IO (Id MTRClusterPumpConfigurationAndControl)
initWithDevice_endpointID_queue mtrClusterPumpConfigurationAndControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterPumpConfigurationAndControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMaxPressureWithParams:@
readAttributeMaxPressureWithParamsSelector :: Selector
readAttributeMaxPressureWithParamsSelector = mkSelector "readAttributeMaxPressureWithParams:"

-- | @Selector@ for @readAttributeMaxSpeedWithParams:@
readAttributeMaxSpeedWithParamsSelector :: Selector
readAttributeMaxSpeedWithParamsSelector = mkSelector "readAttributeMaxSpeedWithParams:"

-- | @Selector@ for @readAttributeMaxFlowWithParams:@
readAttributeMaxFlowWithParamsSelector :: Selector
readAttributeMaxFlowWithParamsSelector = mkSelector "readAttributeMaxFlowWithParams:"

-- | @Selector@ for @readAttributeMinConstPressureWithParams:@
readAttributeMinConstPressureWithParamsSelector :: Selector
readAttributeMinConstPressureWithParamsSelector = mkSelector "readAttributeMinConstPressureWithParams:"

-- | @Selector@ for @readAttributeMaxConstPressureWithParams:@
readAttributeMaxConstPressureWithParamsSelector :: Selector
readAttributeMaxConstPressureWithParamsSelector = mkSelector "readAttributeMaxConstPressureWithParams:"

-- | @Selector@ for @readAttributeMinCompPressureWithParams:@
readAttributeMinCompPressureWithParamsSelector :: Selector
readAttributeMinCompPressureWithParamsSelector = mkSelector "readAttributeMinCompPressureWithParams:"

-- | @Selector@ for @readAttributeMaxCompPressureWithParams:@
readAttributeMaxCompPressureWithParamsSelector :: Selector
readAttributeMaxCompPressureWithParamsSelector = mkSelector "readAttributeMaxCompPressureWithParams:"

-- | @Selector@ for @readAttributeMinConstSpeedWithParams:@
readAttributeMinConstSpeedWithParamsSelector :: Selector
readAttributeMinConstSpeedWithParamsSelector = mkSelector "readAttributeMinConstSpeedWithParams:"

-- | @Selector@ for @readAttributeMaxConstSpeedWithParams:@
readAttributeMaxConstSpeedWithParamsSelector :: Selector
readAttributeMaxConstSpeedWithParamsSelector = mkSelector "readAttributeMaxConstSpeedWithParams:"

-- | @Selector@ for @readAttributeMinConstFlowWithParams:@
readAttributeMinConstFlowWithParamsSelector :: Selector
readAttributeMinConstFlowWithParamsSelector = mkSelector "readAttributeMinConstFlowWithParams:"

-- | @Selector@ for @readAttributeMaxConstFlowWithParams:@
readAttributeMaxConstFlowWithParamsSelector :: Selector
readAttributeMaxConstFlowWithParamsSelector = mkSelector "readAttributeMaxConstFlowWithParams:"

-- | @Selector@ for @readAttributeMinConstTempWithParams:@
readAttributeMinConstTempWithParamsSelector :: Selector
readAttributeMinConstTempWithParamsSelector = mkSelector "readAttributeMinConstTempWithParams:"

-- | @Selector@ for @readAttributeMaxConstTempWithParams:@
readAttributeMaxConstTempWithParamsSelector :: Selector
readAttributeMaxConstTempWithParamsSelector = mkSelector "readAttributeMaxConstTempWithParams:"

-- | @Selector@ for @readAttributePumpStatusWithParams:@
readAttributePumpStatusWithParamsSelector :: Selector
readAttributePumpStatusWithParamsSelector = mkSelector "readAttributePumpStatusWithParams:"

-- | @Selector@ for @readAttributeEffectiveOperationModeWithParams:@
readAttributeEffectiveOperationModeWithParamsSelector :: Selector
readAttributeEffectiveOperationModeWithParamsSelector = mkSelector "readAttributeEffectiveOperationModeWithParams:"

-- | @Selector@ for @readAttributeEffectiveControlModeWithParams:@
readAttributeEffectiveControlModeWithParamsSelector :: Selector
readAttributeEffectiveControlModeWithParamsSelector = mkSelector "readAttributeEffectiveControlModeWithParams:"

-- | @Selector@ for @readAttributeCapacityWithParams:@
readAttributeCapacityWithParamsSelector :: Selector
readAttributeCapacityWithParamsSelector = mkSelector "readAttributeCapacityWithParams:"

-- | @Selector@ for @readAttributeSpeedWithParams:@
readAttributeSpeedWithParamsSelector :: Selector
readAttributeSpeedWithParamsSelector = mkSelector "readAttributeSpeedWithParams:"

-- | @Selector@ for @readAttributeLifetimeRunningHoursWithParams:@
readAttributeLifetimeRunningHoursWithParamsSelector :: Selector
readAttributeLifetimeRunningHoursWithParamsSelector = mkSelector "readAttributeLifetimeRunningHoursWithParams:"

-- | @Selector@ for @writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:@
writeAttributeLifetimeRunningHoursWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLifetimeRunningHoursWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:params:@
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLifetimeRunningHoursWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLifetimeRunningHoursWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePowerWithParams:@
readAttributePowerWithParamsSelector :: Selector
readAttributePowerWithParamsSelector = mkSelector "readAttributePowerWithParams:"

-- | @Selector@ for @readAttributeLifetimeEnergyConsumedWithParams:@
readAttributeLifetimeEnergyConsumedWithParamsSelector :: Selector
readAttributeLifetimeEnergyConsumedWithParamsSelector = mkSelector "readAttributeLifetimeEnergyConsumedWithParams:"

-- | @Selector@ for @writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:@
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:params:@
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLifetimeEnergyConsumedWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLifetimeEnergyConsumedWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOperationModeWithParams:@
readAttributeOperationModeWithParamsSelector :: Selector
readAttributeOperationModeWithParamsSelector = mkSelector "readAttributeOperationModeWithParams:"

-- | @Selector@ for @writeAttributeOperationModeWithValue:expectedValueInterval:@
writeAttributeOperationModeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOperationModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOperationModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOperationModeWithValue:expectedValueInterval:params:@
writeAttributeOperationModeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOperationModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOperationModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeControlModeWithParams:@
readAttributeControlModeWithParamsSelector :: Selector
readAttributeControlModeWithParamsSelector = mkSelector "readAttributeControlModeWithParams:"

-- | @Selector@ for @writeAttributeControlModeWithValue:expectedValueInterval:@
writeAttributeControlModeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeControlModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeControlModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeControlModeWithValue:expectedValueInterval:params:@
writeAttributeControlModeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeControlModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeControlModeWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

