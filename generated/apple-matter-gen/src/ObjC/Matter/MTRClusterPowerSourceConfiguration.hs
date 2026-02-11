{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Power Source Configuration    This cluster is used to describe the configuration and capabilities of a Device's power system.
--
-- Generated bindings for @MTRClusterPowerSourceConfiguration@.
module ObjC.Matter.MTRClusterPowerSourceConfiguration
  ( MTRClusterPowerSourceConfiguration
  , IsMTRClusterPowerSourceConfiguration(..)
  , readAttributeSourcesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeSourcesWithParamsSelector
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

-- | @- readAttributeSourcesWithParams:@
readAttributeSourcesWithParams :: (IsMTRClusterPowerSourceConfiguration mtrClusterPowerSourceConfiguration, IsMTRReadParams params) => mtrClusterPowerSourceConfiguration -> params -> IO (Id NSDictionary)
readAttributeSourcesWithParams mtrClusterPowerSourceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSourceConfiguration (mkSelector "readAttributeSourcesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPowerSourceConfiguration mtrClusterPowerSourceConfiguration, IsMTRReadParams params) => mtrClusterPowerSourceConfiguration -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPowerSourceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSourceConfiguration (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPowerSourceConfiguration mtrClusterPowerSourceConfiguration, IsMTRReadParams params) => mtrClusterPowerSourceConfiguration -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPowerSourceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSourceConfiguration (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPowerSourceConfiguration mtrClusterPowerSourceConfiguration, IsMTRReadParams params) => mtrClusterPowerSourceConfiguration -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPowerSourceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSourceConfiguration (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPowerSourceConfiguration mtrClusterPowerSourceConfiguration, IsMTRReadParams params) => mtrClusterPowerSourceConfiguration -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPowerSourceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSourceConfiguration (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPowerSourceConfiguration mtrClusterPowerSourceConfiguration, IsMTRReadParams params) => mtrClusterPowerSourceConfiguration -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPowerSourceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerSourceConfiguration (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterPowerSourceConfiguration mtrClusterPowerSourceConfiguration => mtrClusterPowerSourceConfiguration -> IO (Id MTRClusterPowerSourceConfiguration)
init_ mtrClusterPowerSourceConfiguration  =
    sendMsg mtrClusterPowerSourceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterPowerSourceConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPowerSourceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterPowerSourceConfiguration mtrClusterPowerSourceConfiguration, IsMTRDevice device, IsNSObject queue) => mtrClusterPowerSourceConfiguration -> device -> CUShort -> queue -> IO (Id MTRClusterPowerSourceConfiguration)
initWithDevice_endpoint_queue mtrClusterPowerSourceConfiguration  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterPowerSourceConfiguration (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPowerSourceConfiguration mtrClusterPowerSourceConfiguration, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPowerSourceConfiguration -> device -> endpointID -> queue -> IO (Id MTRClusterPowerSourceConfiguration)
initWithDevice_endpointID_queue mtrClusterPowerSourceConfiguration  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterPowerSourceConfiguration (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSourcesWithParams:@
readAttributeSourcesWithParamsSelector :: Selector
readAttributeSourcesWithParamsSelector = mkSelector "readAttributeSourcesWithParams:"

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

