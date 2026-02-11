{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Temperature Measurement    Attributes and commands for configuring the measurement of temperature, and reporting temperature measurements.
--
-- Generated bindings for @MTRClusterTemperatureMeasurement@.
module ObjC.Matter.MTRClusterTemperatureMeasurement
  ( MTRClusterTemperatureMeasurement
  , IsMTRClusterTemperatureMeasurement(..)
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
readAttributeMeasuredValueWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredValueWithParams mtrClusterTemperatureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureMeasurement (mkSelector "readAttributeMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinMeasuredValueWithParams:@
readAttributeMinMeasuredValueWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMinMeasuredValueWithParams mtrClusterTemperatureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureMeasurement (mkSelector "readAttributeMinMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxMeasuredValueWithParams:@
readAttributeMaxMeasuredValueWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeMaxMeasuredValueWithParams mtrClusterTemperatureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureMeasurement (mkSelector "readAttributeMaxMeasuredValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeToleranceWithParams:@
readAttributeToleranceWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeToleranceWithParams mtrClusterTemperatureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureMeasurement (mkSelector "readAttributeToleranceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTemperatureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureMeasurement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTemperatureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureMeasurement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTemperatureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureMeasurement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTemperatureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureMeasurement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRReadParams params) => mtrClusterTemperatureMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTemperatureMeasurement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTemperatureMeasurement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement => mtrClusterTemperatureMeasurement -> IO (Id MTRClusterTemperatureMeasurement)
init_ mtrClusterTemperatureMeasurement  =
    sendMsg mtrClusterTemperatureMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterTemperatureMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTemperatureMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRDevice device, IsNSObject queue) => mtrClusterTemperatureMeasurement -> device -> CUShort -> queue -> IO (Id MTRClusterTemperatureMeasurement)
initWithDevice_endpoint_queue mtrClusterTemperatureMeasurement  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterTemperatureMeasurement (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTemperatureMeasurement mtrClusterTemperatureMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTemperatureMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterTemperatureMeasurement)
initWithDevice_endpointID_queue mtrClusterTemperatureMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterTemperatureMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

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

