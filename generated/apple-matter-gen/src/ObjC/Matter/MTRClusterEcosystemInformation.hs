{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Ecosystem Information    Provides extended device information for all the logical devices represented by a Bridged Node.
--
-- Generated bindings for @MTRClusterEcosystemInformation@.
module ObjC.Matter.MTRClusterEcosystemInformation
  ( MTRClusterEcosystemInformation
  , IsMTRClusterEcosystemInformation(..)
  , readAttributeDeviceDirectoryWithParams
  , readAttributeLocationDirectoryWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeDeviceDirectoryWithParamsSelector
  , readAttributeLocationDirectoryWithParamsSelector
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

-- | @- readAttributeDeviceDirectoryWithParams:@
readAttributeDeviceDirectoryWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeDeviceDirectoryWithParams mtrClusterEcosystemInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEcosystemInformation (mkSelector "readAttributeDeviceDirectoryWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLocationDirectoryWithParams:@
readAttributeLocationDirectoryWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeLocationDirectoryWithParams mtrClusterEcosystemInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEcosystemInformation (mkSelector "readAttributeLocationDirectoryWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterEcosystemInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEcosystemInformation (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterEcosystemInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEcosystemInformation (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterEcosystemInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEcosystemInformation (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterEcosystemInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEcosystemInformation (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterEcosystemInformation  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterEcosystemInformation (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation => mtrClusterEcosystemInformation -> IO (Id MTRClusterEcosystemInformation)
init_ mtrClusterEcosystemInformation  =
    sendMsg mtrClusterEcosystemInformation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterEcosystemInformation)
new  =
  do
    cls' <- getRequiredClass "MTRClusterEcosystemInformation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterEcosystemInformation -> device -> endpointID -> queue -> IO (Id MTRClusterEcosystemInformation)
initWithDevice_endpointID_queue mtrClusterEcosystemInformation  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterEcosystemInformation (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeDeviceDirectoryWithParams:@
readAttributeDeviceDirectoryWithParamsSelector :: Selector
readAttributeDeviceDirectoryWithParamsSelector = mkSelector "readAttributeDeviceDirectoryWithParams:"

-- | @Selector@ for @readAttributeLocationDirectoryWithParams:@
readAttributeLocationDirectoryWithParamsSelector :: Selector
readAttributeLocationDirectoryWithParamsSelector = mkSelector "readAttributeLocationDirectoryWithParams:"

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

