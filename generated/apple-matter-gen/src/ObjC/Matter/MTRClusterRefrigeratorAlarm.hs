{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Refrigerator Alarm    Attributes and commands for configuring the Refrigerator alarm.
--
-- Generated bindings for @MTRClusterRefrigeratorAlarm@.
module ObjC.Matter.MTRClusterRefrigeratorAlarm
  ( MTRClusterRefrigeratorAlarm
  , IsMTRClusterRefrigeratorAlarm(..)
  , readAttributeMaskWithParams
  , readAttributeStateWithParams
  , readAttributeSupportedWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeMaskWithParamsSelector
  , readAttributeStateWithParamsSelector
  , readAttributeSupportedWithParamsSelector
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

-- | @- readAttributeMaskWithParams:@
readAttributeMaskWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeMaskWithParams mtrClusterRefrigeratorAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAlarm (mkSelector "readAttributeMaskWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStateWithParams:@
readAttributeStateWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeStateWithParams mtrClusterRefrigeratorAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAlarm (mkSelector "readAttributeStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportedWithParams:@
readAttributeSupportedWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeSupportedWithParams mtrClusterRefrigeratorAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAlarm (mkSelector "readAttributeSupportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterRefrigeratorAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAlarm (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterRefrigeratorAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAlarm (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterRefrigeratorAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAlarm (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterRefrigeratorAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAlarm (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterRefrigeratorAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAlarm (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm => mtrClusterRefrigeratorAlarm -> IO (Id MTRClusterRefrigeratorAlarm)
init_ mtrClusterRefrigeratorAlarm  =
    sendMsg mtrClusterRefrigeratorAlarm (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterRefrigeratorAlarm)
new  =
  do
    cls' <- getRequiredClass "MTRClusterRefrigeratorAlarm"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterRefrigeratorAlarm -> device -> endpointID -> queue -> IO (Id MTRClusterRefrigeratorAlarm)
initWithDevice_endpointID_queue mtrClusterRefrigeratorAlarm  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterRefrigeratorAlarm (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMaskWithParams:@
readAttributeMaskWithParamsSelector :: Selector
readAttributeMaskWithParamsSelector = mkSelector "readAttributeMaskWithParams:"

-- | @Selector@ for @readAttributeStateWithParams:@
readAttributeStateWithParamsSelector :: Selector
readAttributeStateWithParamsSelector = mkSelector "readAttributeStateWithParams:"

-- | @Selector@ for @readAttributeSupportedWithParams:@
readAttributeSupportedWithParamsSelector :: Selector
readAttributeSupportedWithParamsSelector = mkSelector "readAttributeSupportedWithParams:"

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

