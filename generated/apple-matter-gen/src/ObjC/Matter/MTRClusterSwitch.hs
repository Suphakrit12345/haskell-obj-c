{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Switch    This cluster exposes interactions with a switch device, for the purpose of using those interactions by other devices.Two types of switch devices are supported: latching switch (e.g. rocker switch) and momentary switch (e.g. push button), distinguished with their feature flags.Interactions with the switch device are exposed as attributes (for the latching switch) and as events (for both types of switches). An interested party MAY subscribe to these attributes/events and thus be informed of the interactions, and can perform actions based on this, for example by sending commands to perform an action such as controlling a light or a window shade.
--
-- Generated bindings for @MTRClusterSwitch@.
module ObjC.Matter.MTRClusterSwitch
  ( MTRClusterSwitch
  , IsMTRClusterSwitch(..)
  , readAttributeNumberOfPositionsWithParams
  , readAttributeCurrentPositionWithParams
  , readAttributeMultiPressMaxWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeNumberOfPositionsWithParamsSelector
  , readAttributeCurrentPositionWithParamsSelector
  , readAttributeMultiPressMaxWithParamsSelector
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

-- | @- readAttributeNumberOfPositionsWithParams:@
readAttributeNumberOfPositionsWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeNumberOfPositionsWithParams mtrClusterSwitch  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSwitch (mkSelector "readAttributeNumberOfPositionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentPositionWithParams:@
readAttributeCurrentPositionWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionWithParams mtrClusterSwitch  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSwitch (mkSelector "readAttributeCurrentPositionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMultiPressMaxWithParams:@
readAttributeMultiPressMaxWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeMultiPressMaxWithParams mtrClusterSwitch  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSwitch (mkSelector "readAttributeMultiPressMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterSwitch  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSwitch (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterSwitch  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSwitch (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterSwitch  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSwitch (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterSwitch  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSwitch (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterSwitch  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSwitch (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterSwitch mtrClusterSwitch => mtrClusterSwitch -> IO (Id MTRClusterSwitch)
init_ mtrClusterSwitch  =
    sendMsg mtrClusterSwitch (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterSwitch)
new  =
  do
    cls' <- getRequiredClass "MTRClusterSwitch"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRDevice device, IsNSObject queue) => mtrClusterSwitch -> device -> CUShort -> queue -> IO (Id MTRClusterSwitch)
initWithDevice_endpoint_queue mtrClusterSwitch  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterSwitch (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterSwitch -> device -> endpointID -> queue -> IO (Id MTRClusterSwitch)
initWithDevice_endpointID_queue mtrClusterSwitch  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterSwitch (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeNumberOfPositionsWithParams:@
readAttributeNumberOfPositionsWithParamsSelector :: Selector
readAttributeNumberOfPositionsWithParamsSelector = mkSelector "readAttributeNumberOfPositionsWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionWithParams:@
readAttributeCurrentPositionWithParamsSelector :: Selector
readAttributeCurrentPositionWithParamsSelector = mkSelector "readAttributeCurrentPositionWithParams:"

-- | @Selector@ for @readAttributeMultiPressMaxWithParams:@
readAttributeMultiPressMaxWithParamsSelector :: Selector
readAttributeMultiPressMaxWithParamsSelector = mkSelector "readAttributeMultiPressMaxWithParams:"

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

