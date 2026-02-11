{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Microwave Oven Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterMicrowaveOvenMode@.
module ObjC.Matter.MTRClusterMicrowaveOvenMode
  ( MTRClusterMicrowaveOvenMode
  , IsMTRClusterMicrowaveOvenMode(..)
  , readAttributeSupportedModesWithParams
  , readAttributeCurrentModeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeSupportedModesWithParamsSelector
  , readAttributeCurrentModeWithParamsSelector
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

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterMicrowaveOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenMode (mkSelector "readAttributeSupportedModesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterMicrowaveOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenMode (mkSelector "readAttributeCurrentModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMicrowaveOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenMode (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMicrowaveOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenMode (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMicrowaveOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenMode (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMicrowaveOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenMode (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMicrowaveOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMicrowaveOvenMode (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode => mtrClusterMicrowaveOvenMode -> IO (Id MTRClusterMicrowaveOvenMode)
init_ mtrClusterMicrowaveOvenMode  =
    sendMsg mtrClusterMicrowaveOvenMode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterMicrowaveOvenMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMicrowaveOvenMode"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMicrowaveOvenMode -> device -> endpointID -> queue -> IO (Id MTRClusterMicrowaveOvenMode)
initWithDevice_endpointID_queue mtrClusterMicrowaveOvenMode  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterMicrowaveOvenMode (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParamsSelector :: Selector
readAttributeSupportedModesWithParamsSelector = mkSelector "readAttributeSupportedModesWithParams:"

-- | @Selector@ for @readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParamsSelector :: Selector
readAttributeCurrentModeWithParamsSelector = mkSelector "readAttributeCurrentModeWithParams:"

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

