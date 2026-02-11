{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Activated Carbon Filter Monitoring    Attributes and commands for monitoring activated carbon filters in a device
--
-- Generated bindings for @MTRClusterActivatedCarbonFilterMonitoring@.
module ObjC.Matter.MTRClusterActivatedCarbonFilterMonitoring
  ( MTRClusterActivatedCarbonFilterMonitoring
  , IsMTRClusterActivatedCarbonFilterMonitoring(..)
  , resetConditionWithParams_expectedValues_expectedValueInterval_completion
  , resetConditionWithExpectedValues_expectedValueInterval_completion
  , readAttributeConditionWithParams
  , readAttributeDegradationDirectionWithParams
  , readAttributeChangeIndicationWithParams
  , readAttributeInPlaceIndicatorWithParams
  , readAttributeLastChangedTimeWithParams
  , writeAttributeLastChangedTimeWithValue_expectedValueInterval
  , writeAttributeLastChangedTimeWithValue_expectedValueInterval_params
  , readAttributeReplacementProductListWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , resetConditionWithParams_expectedValues_expectedValueInterval_completionSelector
  , resetConditionWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeConditionWithParamsSelector
  , readAttributeDegradationDirectionWithParamsSelector
  , readAttributeChangeIndicationWithParamsSelector
  , readAttributeInPlaceIndicatorWithParamsSelector
  , readAttributeLastChangedTimeWithParamsSelector
  , writeAttributeLastChangedTimeWithValue_expectedValueIntervalSelector
  , writeAttributeLastChangedTimeWithValue_expectedValueInterval_paramsSelector
  , readAttributeReplacementProductListWithParamsSelector
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

-- | @- resetConditionWithParams:expectedValues:expectedValueInterval:completion:@
resetConditionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRActivatedCarbonFilterMonitoringClusterResetConditionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActivatedCarbonFilterMonitoring -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetConditionWithParams_expectedValues_expectedValueInterval_completion mtrClusterActivatedCarbonFilterMonitoring  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "resetConditionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetConditionWithExpectedValues:expectedValueInterval:completion:@
resetConditionWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterActivatedCarbonFilterMonitoring -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetConditionWithExpectedValues_expectedValueInterval_completion mtrClusterActivatedCarbonFilterMonitoring  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "resetConditionWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeConditionWithParams:@
readAttributeConditionWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeConditionWithParams mtrClusterActivatedCarbonFilterMonitoring  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeConditionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDegradationDirectionWithParams:@
readAttributeDegradationDirectionWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeDegradationDirectionWithParams mtrClusterActivatedCarbonFilterMonitoring  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeDegradationDirectionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeChangeIndicationWithParams:@
readAttributeChangeIndicationWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeChangeIndicationWithParams mtrClusterActivatedCarbonFilterMonitoring  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeChangeIndicationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInPlaceIndicatorWithParams:@
readAttributeInPlaceIndicatorWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeInPlaceIndicatorWithParams mtrClusterActivatedCarbonFilterMonitoring  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeInPlaceIndicatorWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLastChangedTimeWithParams:@
readAttributeLastChangedTimeWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeLastChangedTimeWithParams mtrClusterActivatedCarbonFilterMonitoring  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeLastChangedTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLastChangedTimeWithValue:expectedValueInterval:@
writeAttributeLastChangedTimeWithValue_expectedValueInterval :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterActivatedCarbonFilterMonitoring -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLastChangedTimeWithValue_expectedValueInterval mtrClusterActivatedCarbonFilterMonitoring  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "writeAttributeLastChangedTimeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLastChangedTimeWithValue:expectedValueInterval:params:@
writeAttributeLastChangedTimeWithValue_expectedValueInterval_params :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterActivatedCarbonFilterMonitoring -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLastChangedTimeWithValue_expectedValueInterval_params mtrClusterActivatedCarbonFilterMonitoring  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "writeAttributeLastChangedTimeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeReplacementProductListWithParams:@
readAttributeReplacementProductListWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeReplacementProductListWithParams mtrClusterActivatedCarbonFilterMonitoring  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeReplacementProductListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterActivatedCarbonFilterMonitoring  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterActivatedCarbonFilterMonitoring  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterActivatedCarbonFilterMonitoring  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterActivatedCarbonFilterMonitoring  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterActivatedCarbonFilterMonitoring  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring => mtrClusterActivatedCarbonFilterMonitoring -> IO (Id MTRClusterActivatedCarbonFilterMonitoring)
init_ mtrClusterActivatedCarbonFilterMonitoring  =
    sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterActivatedCarbonFilterMonitoring)
new  =
  do
    cls' <- getRequiredClass "MTRClusterActivatedCarbonFilterMonitoring"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterActivatedCarbonFilterMonitoring -> device -> endpointID -> queue -> IO (Id MTRClusterActivatedCarbonFilterMonitoring)
initWithDevice_endpointID_queue mtrClusterActivatedCarbonFilterMonitoring  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterActivatedCarbonFilterMonitoring (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetConditionWithParams:expectedValues:expectedValueInterval:completion:@
resetConditionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resetConditionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetConditionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetConditionWithExpectedValues:expectedValueInterval:completion:@
resetConditionWithExpectedValues_expectedValueInterval_completionSelector :: Selector
resetConditionWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetConditionWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeConditionWithParams:@
readAttributeConditionWithParamsSelector :: Selector
readAttributeConditionWithParamsSelector = mkSelector "readAttributeConditionWithParams:"

-- | @Selector@ for @readAttributeDegradationDirectionWithParams:@
readAttributeDegradationDirectionWithParamsSelector :: Selector
readAttributeDegradationDirectionWithParamsSelector = mkSelector "readAttributeDegradationDirectionWithParams:"

-- | @Selector@ for @readAttributeChangeIndicationWithParams:@
readAttributeChangeIndicationWithParamsSelector :: Selector
readAttributeChangeIndicationWithParamsSelector = mkSelector "readAttributeChangeIndicationWithParams:"

-- | @Selector@ for @readAttributeInPlaceIndicatorWithParams:@
readAttributeInPlaceIndicatorWithParamsSelector :: Selector
readAttributeInPlaceIndicatorWithParamsSelector = mkSelector "readAttributeInPlaceIndicatorWithParams:"

-- | @Selector@ for @readAttributeLastChangedTimeWithParams:@
readAttributeLastChangedTimeWithParamsSelector :: Selector
readAttributeLastChangedTimeWithParamsSelector = mkSelector "readAttributeLastChangedTimeWithParams:"

-- | @Selector@ for @writeAttributeLastChangedTimeWithValue:expectedValueInterval:@
writeAttributeLastChangedTimeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLastChangedTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLastChangedTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLastChangedTimeWithValue:expectedValueInterval:params:@
writeAttributeLastChangedTimeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLastChangedTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLastChangedTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeReplacementProductListWithParams:@
readAttributeReplacementProductListWithParamsSelector :: Selector
readAttributeReplacementProductListWithParamsSelector = mkSelector "readAttributeReplacementProductListWithParams:"

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

