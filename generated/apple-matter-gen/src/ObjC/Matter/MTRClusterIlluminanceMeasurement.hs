{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Illuminance Measurement    Attributes and commands for configuring the measurement of illuminance, and reporting illuminance measurements.
--
-- Generated bindings for @MTRClusterIlluminanceMeasurement@.
module ObjC.Matter.MTRClusterIlluminanceMeasurement
  ( MTRClusterIlluminanceMeasurement
  , IsMTRClusterIlluminanceMeasurement(..)
  , readAttributeMeasuredValueWithParams
  , readAttributeMinMeasuredValueWithParams
  , readAttributeMaxMeasuredValueWithParams
  , readAttributeToleranceWithParams
  , readAttributeLightSensorTypeWithParams
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
  , readAttributeLightSensorTypeWithParamsSelector
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterIlluminanceMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIlluminanceMeasurement (mkSelector "readAttributeMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterIlluminanceMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIlluminanceMeasurement (mkSelector "readAttributeMinMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterIlluminanceMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIlluminanceMeasurement (mkSelector "readAttributeMaxMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeToleranceWithParams:@
readAttributeToleranceWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeToleranceWithParams mtrClusterIlluminanceMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIlluminanceMeasurement (mkSelector "readAttributeToleranceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLightSensorTypeWithParams:@
readAttributeLightSensorTypeWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeLightSensorTypeWithParams mtrClusterIlluminanceMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIlluminanceMeasurement (mkSelector "readAttributeLightSensorTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterIlluminanceMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIlluminanceMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterIlluminanceMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIlluminanceMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterIlluminanceMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIlluminanceMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterIlluminanceMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIlluminanceMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRReadParams params) => mtrClusterIlluminanceMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterIlluminanceMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterIlluminanceMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement => mtrClusterIlluminanceMeasurement -> IO (Id MTRClusterIlluminanceMeasurement)
init_ mtrClusterIlluminanceMeasurement  =
    sendMsg mtrClusterIlluminanceMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterIlluminanceMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterIlluminanceMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRDevice device, IsNSObject queue) => mtrClusterIlluminanceMeasurement -> device -> CUShort -> queue -> IO (Id MTRClusterIlluminanceMeasurement)
initWithDevice_endpoint_queue mtrClusterIlluminanceMeasurement  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterIlluminanceMeasurement (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterIlluminanceMeasurement mtrClusterIlluminanceMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterIlluminanceMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterIlluminanceMeasurement)
initWithDevice_endpointID_queue mtrClusterIlluminanceMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterIlluminanceMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

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

-- | @Selector@ for @readAttributeLightSensorTypeWithParams:@
readAttributeLightSensorTypeWithParamsSelector :: Selector
readAttributeLightSensorTypeWithParamsSelector = mkSelector "readAttributeLightSensorTypeWithParams:"

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

