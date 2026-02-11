{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Boolean State    This cluster provides an interface to a boolean state called StateValue.
--
-- Generated bindings for @MTRClusterBooleanState@.
module ObjC.Matter.MTRClusterBooleanState
  ( MTRClusterBooleanState
  , IsMTRClusterBooleanState(..)
  , readAttributeStateValueWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeStateValueWithParamsSelector
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

-- | @- readAttributeStateValueWithParams:@
readAttributeStateValueWithParams :: (IsMTRClusterBooleanState mtrClusterBooleanState, IsMTRReadParams params) => mtrClusterBooleanState -> params -> IO (Id NSDictionary)
readAttributeStateValueWithParams mtrClusterBooleanState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanState (mkSelector "readAttributeStateValueWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBooleanState mtrClusterBooleanState, IsMTRReadParams params) => mtrClusterBooleanState -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBooleanState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanState (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBooleanState mtrClusterBooleanState, IsMTRReadParams params) => mtrClusterBooleanState -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBooleanState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanState (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBooleanState mtrClusterBooleanState, IsMTRReadParams params) => mtrClusterBooleanState -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBooleanState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanState (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBooleanState mtrClusterBooleanState, IsMTRReadParams params) => mtrClusterBooleanState -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBooleanState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanState (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBooleanState mtrClusterBooleanState, IsMTRReadParams params) => mtrClusterBooleanState -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBooleanState  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanState (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterBooleanState mtrClusterBooleanState => mtrClusterBooleanState -> IO (Id MTRClusterBooleanState)
init_ mtrClusterBooleanState  =
    sendMsg mtrClusterBooleanState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterBooleanState)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBooleanState"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterBooleanState mtrClusterBooleanState, IsMTRDevice device, IsNSObject queue) => mtrClusterBooleanState -> device -> CUShort -> queue -> IO (Id MTRClusterBooleanState)
initWithDevice_endpoint_queue mtrClusterBooleanState  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterBooleanState (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBooleanState mtrClusterBooleanState, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBooleanState -> device -> endpointID -> queue -> IO (Id MTRClusterBooleanState)
initWithDevice_endpointID_queue mtrClusterBooleanState  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterBooleanState (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeStateValueWithParams:@
readAttributeStateValueWithParamsSelector :: Selector
readAttributeStateValueWithParamsSelector = mkSelector "readAttributeStateValueWithParams:"

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

