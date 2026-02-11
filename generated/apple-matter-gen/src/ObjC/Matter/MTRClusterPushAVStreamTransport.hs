{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Push AV Stream Transport    This cluster implements the upload of Audio and Video streams from the Push AV Stream Transport Cluster using suitable push-based transports.
--
-- Generated bindings for @MTRClusterPushAVStreamTransport@.
module ObjC.Matter.MTRClusterPushAVStreamTransport
  ( MTRClusterPushAVStreamTransport
  , IsMTRClusterPushAVStreamTransport(..)
  , allocatePushTransportWithParams_expectedValues_expectedValueInterval_completion
  , deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completion
  , modifyPushTransportWithParams_expectedValues_expectedValueInterval_completion
  , setTransportStatusWithParams_expectedValues_expectedValueInterval_completion
  , manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completion
  , findTransportWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSupportedFormatsWithParams
  , readAttributeCurrentConnectionsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , allocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector
  , deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector
  , modifyPushTransportWithParams_expectedValues_expectedValueInterval_completionSelector
  , setTransportStatusWithParams_expectedValues_expectedValueInterval_completionSelector
  , manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completionSelector
  , findTransportWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeSupportedFormatsWithParamsSelector
  , readAttributeCurrentConnectionsWithParamsSelector
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

-- | @- allocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:@
allocatePushTransportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterAllocatePushTransportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
allocatePushTransportWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterPushAVStreamTransport (mkSelector "allocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- deallocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:@
deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterDeallocatePushTransportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterPushAVStreamTransport (mkSelector "deallocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- modifyPushTransportWithParams:expectedValues:expectedValueInterval:completion:@
modifyPushTransportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterModifyPushTransportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
modifyPushTransportWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterPushAVStreamTransport (mkSelector "modifyPushTransportWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setTransportStatusWithParams:expectedValues:expectedValueInterval:completion:@
setTransportStatusWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterSetTransportStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTransportStatusWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterPushAVStreamTransport (mkSelector "setTransportStatusWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- manuallyTriggerTransportWithParams:expectedValues:expectedValueInterval:completion:@
manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterPushAVStreamTransport (mkSelector "manuallyTriggerTransportWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- findTransportWithParams:expectedValues:expectedValueInterval:completion:@
findTransportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterFindTransportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
findTransportWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterPushAVStreamTransport (mkSelector "findTransportWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedFormatsWithParams:@
readAttributeSupportedFormatsWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeSupportedFormatsWithParams mtrClusterPushAVStreamTransport  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPushAVStreamTransport (mkSelector "readAttributeSupportedFormatsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentConnectionsWithParams:@
readAttributeCurrentConnectionsWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeCurrentConnectionsWithParams mtrClusterPushAVStreamTransport  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPushAVStreamTransport (mkSelector "readAttributeCurrentConnectionsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPushAVStreamTransport  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPushAVStreamTransport (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPushAVStreamTransport  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPushAVStreamTransport (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPushAVStreamTransport  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPushAVStreamTransport (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPushAVStreamTransport  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPushAVStreamTransport (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPushAVStreamTransport  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPushAVStreamTransport (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport => mtrClusterPushAVStreamTransport -> IO (Id MTRClusterPushAVStreamTransport)
init_ mtrClusterPushAVStreamTransport  =
    sendMsg mtrClusterPushAVStreamTransport (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterPushAVStreamTransport)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPushAVStreamTransport"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPushAVStreamTransport -> device -> endpointID -> queue -> IO (Id MTRClusterPushAVStreamTransport)
initWithDevice_endpointID_queue mtrClusterPushAVStreamTransport  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterPushAVStreamTransport (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:@
allocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
allocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "allocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @deallocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:@
deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "deallocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @modifyPushTransportWithParams:expectedValues:expectedValueInterval:completion:@
modifyPushTransportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
modifyPushTransportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "modifyPushTransportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTransportStatusWithParams:expectedValues:expectedValueInterval:completion:@
setTransportStatusWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setTransportStatusWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTransportStatusWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @manuallyTriggerTransportWithParams:expectedValues:expectedValueInterval:completion:@
manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "manuallyTriggerTransportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @findTransportWithParams:expectedValues:expectedValueInterval:completion:@
findTransportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
findTransportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "findTransportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSupportedFormatsWithParams:@
readAttributeSupportedFormatsWithParamsSelector :: Selector
readAttributeSupportedFormatsWithParamsSelector = mkSelector "readAttributeSupportedFormatsWithParams:"

-- | @Selector@ for @readAttributeCurrentConnectionsWithParams:@
readAttributeCurrentConnectionsWithParamsSelector :: Selector
readAttributeCurrentConnectionsWithParamsSelector = mkSelector "readAttributeCurrentConnectionsWithParams:"

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

