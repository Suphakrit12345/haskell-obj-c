{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Pressure Measurement    Attributes and commands for configuring the measurement of pressure, and reporting pressure measurements.
--
-- Generated bindings for @MTRClusterPressureMeasurement@.
module ObjC.Matter.MTRClusterPressureMeasurement
  ( MTRClusterPressureMeasurement
  , IsMTRClusterPressureMeasurement(..)
  , readAttributeMeasuredValueWithParams
  , readAttributeMinMeasuredValueWithParams
  , readAttributeMaxMeasuredValueWithParams
  , readAttributeToleranceWithParams
  , readAttributeScaledValueWithParams
  , readAttributeMinScaledValueWithParams
  , readAttributeMaxScaledValueWithParams
  , readAttributeScaledToleranceWithParams
  , readAttributeScaleWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeMeasuredValueWithParamsSelector
  , readAttributeMinMeasuredValueWithParamsSelector
  , readAttributeMaxMeasuredValueWithParamsSelector
  , readAttributeToleranceWithParamsSelector
  , readAttributeScaledValueWithParamsSelector
  , readAttributeMinScaledValueWithParamsSelector
  , readAttributeMaxScaledValueWithParamsSelector
  , readAttributeScaledToleranceWithParamsSelector
  , readAttributeScaleWithParamsSelector
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

-- | @- readAttributeMeasuredValueWithParams:@
readAttributeMeasuredValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeMinMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeMaxMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeToleranceWithParams:@
readAttributeToleranceWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeToleranceWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeToleranceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeScaledValueWithParams:@
readAttributeScaledValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeScaledValueWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeScaledValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinScaledValueWithParams:@
readAttributeMinScaledValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinScaledValueWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeMinScaledValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxScaledValueWithParams:@
readAttributeMaxScaledValueWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxScaledValueWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeMaxScaledValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeScaledToleranceWithParams:@
readAttributeScaledToleranceWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeScaledToleranceWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeScaledToleranceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeScaleWithParams:@
readAttributeScaleWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeScaleWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeScaleWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRReadParams params) => mtrClusterPressureMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPressureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPressureMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement => mtrClusterPressureMeasurement -> IO (Id MTRClusterPressureMeasurement)
init_ mtrClusterPressureMeasurement  =
    sendMsg mtrClusterPressureMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterPressureMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPressureMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRDevice device, IsNSObject queue) => mtrClusterPressureMeasurement -> device -> CUShort -> queue -> IO (Id MTRClusterPressureMeasurement)
initWithDevice_endpoint_queue mtrClusterPressureMeasurement  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterPressureMeasurement (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPressureMeasurement mtrClusterPressureMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPressureMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterPressureMeasurement)
initWithDevice_endpointID_queue mtrClusterPressureMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterPressureMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeasuredValueWithParams:@
readAttributeMeasuredValueWithParamsSelector :: Selector
readAttributeMeasuredValueWithParamsSelector = mkSelector "readAttributeMeasuredValueWithParams:"

-- | @Selector@ for @readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParamsSelector :: Selector
readAttributeMinMeasuredValueWithParamsSelector = mkSelector "readAttributeMinMeasuredValueWithParams:"

-- | @Selector@ for @readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParamsSelector :: Selector
readAttributeMaxMeasuredValueWithParamsSelector = mkSelector "readAttributeMaxMeasuredValueWithParams:"

-- | @Selector@ for @readAttributeToleranceWithParams:@
readAttributeToleranceWithParamsSelector :: Selector
readAttributeToleranceWithParamsSelector = mkSelector "readAttributeToleranceWithParams:"

-- | @Selector@ for @readAttributeScaledValueWithParams:@
readAttributeScaledValueWithParamsSelector :: Selector
readAttributeScaledValueWithParamsSelector = mkSelector "readAttributeScaledValueWithParams:"

-- | @Selector@ for @readAttributeMinScaledValueWithParams:@
readAttributeMinScaledValueWithParamsSelector :: Selector
readAttributeMinScaledValueWithParamsSelector = mkSelector "readAttributeMinScaledValueWithParams:"

-- | @Selector@ for @readAttributeMaxScaledValueWithParams:@
readAttributeMaxScaledValueWithParamsSelector :: Selector
readAttributeMaxScaledValueWithParamsSelector = mkSelector "readAttributeMaxScaledValueWithParams:"

-- | @Selector@ for @readAttributeScaledToleranceWithParams:@
readAttributeScaledToleranceWithParamsSelector :: Selector
readAttributeScaledToleranceWithParamsSelector = mkSelector "readAttributeScaledToleranceWithParams:"

-- | @Selector@ for @readAttributeScaleWithParams:@
readAttributeScaleWithParamsSelector :: Selector
readAttributeScaleWithParamsSelector = mkSelector "readAttributeScaleWithParams:"

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

