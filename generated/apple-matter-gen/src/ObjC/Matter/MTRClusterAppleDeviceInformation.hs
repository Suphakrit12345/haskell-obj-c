{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Apple Device Information    This cluster provides Apple-specific information about the device.
--
-- Generated bindings for @MTRClusterAppleDeviceInformation@.
module ObjC.Matter.MTRClusterAppleDeviceInformation
  ( MTRClusterAppleDeviceInformation
  , IsMTRClusterAppleDeviceInformation(..)
  , readAttributeSupportsTapToUnlockWithParams
  , readAttributeSupportsWEDWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeSupportsTapToUnlockWithParamsSelector
  , readAttributeSupportsWEDWithParamsSelector
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

-- | @- readAttributeSupportsTapToUnlockWithParams:@
readAttributeSupportsTapToUnlockWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeSupportsTapToUnlockWithParams mtrClusterAppleDeviceInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleDeviceInformation (mkSelector "readAttributeSupportsTapToUnlockWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportsWEDWithParams:@
readAttributeSupportsWEDWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeSupportsWEDWithParams mtrClusterAppleDeviceInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleDeviceInformation (mkSelector "readAttributeSupportsWEDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAppleDeviceInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleDeviceInformation (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAppleDeviceInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleDeviceInformation (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAppleDeviceInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleDeviceInformation (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAppleDeviceInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleDeviceInformation (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAppleDeviceInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleDeviceInformation (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation => mtrClusterAppleDeviceInformation -> IO (Id MTRClusterAppleDeviceInformation)
init_ mtrClusterAppleDeviceInformation  =
    sendMsg mtrClusterAppleDeviceInformation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterAppleDeviceInformation)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAppleDeviceInformation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAppleDeviceInformation -> device -> endpointID -> queue -> IO (Id MTRClusterAppleDeviceInformation)
initWithDevice_endpointID_queue mtrClusterAppleDeviceInformation  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterAppleDeviceInformation (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSupportsTapToUnlockWithParams:@
readAttributeSupportsTapToUnlockWithParamsSelector :: Selector
readAttributeSupportsTapToUnlockWithParamsSelector = mkSelector "readAttributeSupportsTapToUnlockWithParams:"

-- | @Selector@ for @readAttributeSupportsWEDWithParams:@
readAttributeSupportsWEDWithParamsSelector :: Selector
readAttributeSupportsWEDWithParamsSelector = mkSelector "readAttributeSupportsWEDWithParams:"

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

