{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Meter Identification    This Meter Identification Cluster provides attributes for determining advanced information about utility metering device.
--
-- Generated bindings for @MTRClusterMeterIdentification@.
module ObjC.Matter.MTRClusterMeterIdentification
  ( MTRClusterMeterIdentification
  , IsMTRClusterMeterIdentification(..)
  , readAttributeMeterTypeWithParams
  , readAttributePointOfDeliveryWithParams
  , readAttributeMeterSerialNumberWithParams
  , readAttributeProtocolVersionWithParams
  , readAttributePowerThresholdWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeMeterTypeWithParamsSelector
  , readAttributePointOfDeliveryWithParamsSelector
  , readAttributeMeterSerialNumberWithParamsSelector
  , readAttributeProtocolVersionWithParamsSelector
  , readAttributePowerThresholdWithParamsSelector
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

-- | @- readAttributeMeterTypeWithParams:@
readAttributeMeterTypeWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeMeterTypeWithParams mtrClusterMeterIdentification  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMeterIdentification (mkSelector "readAttributeMeterTypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePointOfDeliveryWithParams:@
readAttributePointOfDeliveryWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributePointOfDeliveryWithParams mtrClusterMeterIdentification  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMeterIdentification (mkSelector "readAttributePointOfDeliveryWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeterSerialNumberWithParams:@
readAttributeMeterSerialNumberWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeMeterSerialNumberWithParams mtrClusterMeterIdentification  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMeterIdentification (mkSelector "readAttributeMeterSerialNumberWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProtocolVersionWithParams:@
readAttributeProtocolVersionWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeProtocolVersionWithParams mtrClusterMeterIdentification  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMeterIdentification (mkSelector "readAttributeProtocolVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePowerThresholdWithParams:@
readAttributePowerThresholdWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributePowerThresholdWithParams mtrClusterMeterIdentification  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMeterIdentification (mkSelector "readAttributePowerThresholdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMeterIdentification  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMeterIdentification (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMeterIdentification  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMeterIdentification (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMeterIdentification  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMeterIdentification (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMeterIdentification  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMeterIdentification (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMeterIdentification  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMeterIdentification (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterMeterIdentification mtrClusterMeterIdentification => mtrClusterMeterIdentification -> IO (Id MTRClusterMeterIdentification)
init_ mtrClusterMeterIdentification  =
    sendMsg mtrClusterMeterIdentification (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterMeterIdentification)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMeterIdentification"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMeterIdentification -> device -> endpointID -> queue -> IO (Id MTRClusterMeterIdentification)
initWithDevice_endpointID_queue mtrClusterMeterIdentification  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterMeterIdentification (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeterTypeWithParams:@
readAttributeMeterTypeWithParamsSelector :: Selector
readAttributeMeterTypeWithParamsSelector = mkSelector "readAttributeMeterTypeWithParams:"

-- | @Selector@ for @readAttributePointOfDeliveryWithParams:@
readAttributePointOfDeliveryWithParamsSelector :: Selector
readAttributePointOfDeliveryWithParamsSelector = mkSelector "readAttributePointOfDeliveryWithParams:"

-- | @Selector@ for @readAttributeMeterSerialNumberWithParams:@
readAttributeMeterSerialNumberWithParamsSelector :: Selector
readAttributeMeterSerialNumberWithParamsSelector = mkSelector "readAttributeMeterSerialNumberWithParams:"

-- | @Selector@ for @readAttributeProtocolVersionWithParams:@
readAttributeProtocolVersionWithParamsSelector :: Selector
readAttributeProtocolVersionWithParamsSelector = mkSelector "readAttributeProtocolVersionWithParams:"

-- | @Selector@ for @readAttributePowerThresholdWithParams:@
readAttributePowerThresholdWithParamsSelector :: Selector
readAttributePowerThresholdWithParamsSelector = mkSelector "readAttributePowerThresholdWithParams:"

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

