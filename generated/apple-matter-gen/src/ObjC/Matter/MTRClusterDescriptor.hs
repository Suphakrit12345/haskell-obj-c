{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Descriptor    The Descriptor Cluster is meant to replace the support from the Zigbee Device Object (ZDO) for describing a node, its endpoints and clusters.
--
-- Generated bindings for @MTRClusterDescriptor@.
module ObjC.Matter.MTRClusterDescriptor
  ( MTRClusterDescriptor
  , IsMTRClusterDescriptor(..)
  , readAttributeDeviceTypeListWithParams
  , readAttributeServerListWithParams
  , readAttributeClientListWithParams
  , readAttributePartsListWithParams
  , readAttributeTagListWithParams
  , readAttributeEndpointUniqueIDWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , readAttributeDeviceListWithParams
  , initWithDevice_endpointID_queue
  , readAttributeDeviceTypeListWithParamsSelector
  , readAttributeServerListWithParamsSelector
  , readAttributeClientListWithParamsSelector
  , readAttributePartsListWithParamsSelector
  , readAttributeTagListWithParamsSelector
  , readAttributeEndpointUniqueIDWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , readAttributeDeviceListWithParamsSelector
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

-- | @- readAttributeDeviceTypeListWithParams:@
readAttributeDeviceTypeListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeDeviceTypeListWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributeDeviceTypeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeServerListWithParams:@
readAttributeServerListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeServerListWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributeServerListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClientListWithParams:@
readAttributeClientListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeClientListWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributeClientListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePartsListWithParams:@
readAttributePartsListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributePartsListWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributePartsListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTagListWithParams:@
readAttributeTagListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeTagListWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributeTagListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEndpointUniqueIDWithParams:@
readAttributeEndpointUniqueIDWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeEndpointUniqueIDWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributeEndpointUniqueIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterDescriptor mtrClusterDescriptor => mtrClusterDescriptor -> IO (Id MTRClusterDescriptor)
init_ mtrClusterDescriptor  =
    sendMsg mtrClusterDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterDescriptor)
new  =
  do
    cls' <- getRequiredClass "MTRClusterDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRDevice device, IsNSObject queue) => mtrClusterDescriptor -> device -> CUShort -> queue -> IO (Id MTRClusterDescriptor)
initWithDevice_endpoint_queue mtrClusterDescriptor  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterDescriptor (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeDeviceListWithParams:@
readAttributeDeviceListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeDeviceListWithParams mtrClusterDescriptor  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDescriptor (mkSelector "readAttributeDeviceListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterDescriptor -> device -> endpointID -> queue -> IO (Id MTRClusterDescriptor)
initWithDevice_endpointID_queue mtrClusterDescriptor  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterDescriptor (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeDeviceTypeListWithParams:@
readAttributeDeviceTypeListWithParamsSelector :: Selector
readAttributeDeviceTypeListWithParamsSelector = mkSelector "readAttributeDeviceTypeListWithParams:"

-- | @Selector@ for @readAttributeServerListWithParams:@
readAttributeServerListWithParamsSelector :: Selector
readAttributeServerListWithParamsSelector = mkSelector "readAttributeServerListWithParams:"

-- | @Selector@ for @readAttributeClientListWithParams:@
readAttributeClientListWithParamsSelector :: Selector
readAttributeClientListWithParamsSelector = mkSelector "readAttributeClientListWithParams:"

-- | @Selector@ for @readAttributePartsListWithParams:@
readAttributePartsListWithParamsSelector :: Selector
readAttributePartsListWithParamsSelector = mkSelector "readAttributePartsListWithParams:"

-- | @Selector@ for @readAttributeTagListWithParams:@
readAttributeTagListWithParamsSelector :: Selector
readAttributeTagListWithParamsSelector = mkSelector "readAttributeTagListWithParams:"

-- | @Selector@ for @readAttributeEndpointUniqueIDWithParams:@
readAttributeEndpointUniqueIDWithParamsSelector :: Selector
readAttributeEndpointUniqueIDWithParamsSelector = mkSelector "readAttributeEndpointUniqueIDWithParams:"

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

-- | @Selector@ for @readAttributeDeviceListWithParams:@
readAttributeDeviceListWithParamsSelector :: Selector
readAttributeDeviceListWithParamsSelector = mkSelector "readAttributeDeviceListWithParams:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

