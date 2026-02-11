{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Relative Humidity Measurement    Attributes and commands for configuring the measurement of relative humidity, and reporting relative humidity measurements.
--
-- Generated bindings for @MTRClusterRelativeHumidityMeasurement@.
module ObjC.Matter.MTRClusterRelativeHumidityMeasurement
  ( MTRClusterRelativeHumidityMeasurement
  , IsMTRClusterRelativeHumidityMeasurement(..)
  , readAttributeMeasuredValueWithParams
  , readAttributeMinMeasuredValueWithParams
  , readAttributeMaxMeasuredValueWithParams
  , readAttributeToleranceWithParams
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterRelativeHumidityMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "readAttributeMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterRelativeHumidityMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "readAttributeMinMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterRelativeHumidityMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "readAttributeMaxMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeToleranceWithParams:@
readAttributeToleranceWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeToleranceWithParams mtrClusterRelativeHumidityMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "readAttributeToleranceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterRelativeHumidityMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterRelativeHumidityMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterRelativeHumidityMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterRelativeHumidityMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRReadParams params) => mtrClusterRelativeHumidityMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterRelativeHumidityMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement => mtrClusterRelativeHumidityMeasurement -> IO (Id MTRClusterRelativeHumidityMeasurement)
init_ mtrClusterRelativeHumidityMeasurement  =
    sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterRelativeHumidityMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterRelativeHumidityMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRDevice device, IsNSObject queue) => mtrClusterRelativeHumidityMeasurement -> device -> CUShort -> queue -> IO (Id MTRClusterRelativeHumidityMeasurement)
initWithDevice_endpoint_queue mtrClusterRelativeHumidityMeasurement  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterRelativeHumidityMeasurement mtrClusterRelativeHumidityMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterRelativeHumidityMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterRelativeHumidityMeasurement)
initWithDevice_endpointID_queue mtrClusterRelativeHumidityMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterRelativeHumidityMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

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

