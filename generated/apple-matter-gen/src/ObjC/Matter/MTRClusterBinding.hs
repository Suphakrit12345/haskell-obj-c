{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Binding    The Binding Cluster is meant to replace the support from the Zigbee Device Object (ZDO) for supporting the binding table.
--
-- Generated bindings for @MTRClusterBinding@.
module ObjC.Matter.MTRClusterBinding
  ( MTRClusterBinding
  , IsMTRClusterBinding(..)
  , readAttributeBindingWithParams
  , writeAttributeBindingWithValue_expectedValueInterval
  , writeAttributeBindingWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeBindingWithParamsSelector
  , writeAttributeBindingWithValue_expectedValueIntervalSelector
  , writeAttributeBindingWithValue_expectedValueInterval_paramsSelector
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

-- | @- readAttributeBindingWithParams:@
readAttributeBindingWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeBindingWithParams mtrClusterBinding  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinding (mkSelector "readAttributeBindingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeBindingWithValue:expectedValueInterval:@
writeAttributeBindingWithValue_expectedValueInterval :: (IsMTRClusterBinding mtrClusterBinding, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinding -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBindingWithValue_expectedValueInterval mtrClusterBinding  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBinding (mkSelector "writeAttributeBindingWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeBindingWithValue:expectedValueInterval:params:@
writeAttributeBindingWithValue_expectedValueInterval_params :: (IsMTRClusterBinding mtrClusterBinding, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinding -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBindingWithValue_expectedValueInterval_params mtrClusterBinding  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBinding (mkSelector "writeAttributeBindingWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBinding  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinding (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBinding  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinding (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBinding  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinding (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBinding  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinding (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBinding  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBinding (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterBinding mtrClusterBinding => mtrClusterBinding -> IO (Id MTRClusterBinding)
init_ mtrClusterBinding  =
    sendMsg mtrClusterBinding (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterBinding)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBinding"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterBinding mtrClusterBinding, IsMTRDevice device, IsNSObject queue) => mtrClusterBinding -> device -> CUShort -> queue -> IO (Id MTRClusterBinding)
initWithDevice_endpoint_queue mtrClusterBinding  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterBinding (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBinding mtrClusterBinding, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBinding -> device -> endpointID -> queue -> IO (Id MTRClusterBinding)
initWithDevice_endpointID_queue mtrClusterBinding  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterBinding (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeBindingWithParams:@
readAttributeBindingWithParamsSelector :: Selector
readAttributeBindingWithParamsSelector = mkSelector "readAttributeBindingWithParams:"

-- | @Selector@ for @writeAttributeBindingWithValue:expectedValueInterval:@
writeAttributeBindingWithValue_expectedValueIntervalSelector :: Selector
writeAttributeBindingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBindingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBindingWithValue:expectedValueInterval:params:@
writeAttributeBindingWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeBindingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBindingWithValue:expectedValueInterval:params:"

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

