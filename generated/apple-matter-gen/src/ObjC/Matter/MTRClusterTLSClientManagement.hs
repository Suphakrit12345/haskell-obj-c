{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster TLS Client Management    This Cluster is used to provision TLS Endpoints with enough information to facilitate subsequent connection.
--
-- Generated bindings for @MTRClusterTLSClientManagement@.
module ObjC.Matter.MTRClusterTLSClientManagement
  ( MTRClusterTLSClientManagement
  , IsMTRClusterTLSClientManagement(..)
  , provisionEndpointWithParams_expectedValues_expectedValueInterval_completion
  , findEndpointWithParams_expectedValues_expectedValueInterval_completion
  , removeEndpointWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMaxProvisionedWithParams
  , readAttributeProvisionedEndpointsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , provisionEndpointWithParams_expectedValues_expectedValueInterval_completionSelector
  , findEndpointWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeEndpointWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeMaxProvisionedWithParamsSelector
  , readAttributeProvisionedEndpointsWithParamsSelector
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

-- | @- provisionEndpointWithParams:expectedValues:expectedValueInterval:completion:@
provisionEndpointWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRTLSClientManagementClusterProvisionEndpointParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSClientManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provisionEndpointWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSClientManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSClientManagement (mkSelector "provisionEndpointWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- findEndpointWithParams:expectedValues:expectedValueInterval:completion:@
findEndpointWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRTLSClientManagementClusterFindEndpointParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSClientManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
findEndpointWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSClientManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSClientManagement (mkSelector "findEndpointWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeEndpointWithParams:expectedValues:expectedValueInterval:completion:@
removeEndpointWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRTLSClientManagementClusterRemoveEndpointParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSClientManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeEndpointWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSClientManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSClientManagement (mkSelector "removeEndpointWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxProvisionedWithParams:@
readAttributeMaxProvisionedWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeMaxProvisionedWithParams mtrClusterTLSClientManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSClientManagement (mkSelector "readAttributeMaxProvisionedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProvisionedEndpointsWithParams:@
readAttributeProvisionedEndpointsWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeProvisionedEndpointsWithParams mtrClusterTLSClientManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSClientManagement (mkSelector "readAttributeProvisionedEndpointsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTLSClientManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSClientManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTLSClientManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSClientManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTLSClientManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSClientManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTLSClientManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSClientManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTLSClientManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSClientManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement => mtrClusterTLSClientManagement -> IO (Id MTRClusterTLSClientManagement)
init_ mtrClusterTLSClientManagement  =
    sendMsg mtrClusterTLSClientManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterTLSClientManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTLSClientManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTLSClientManagement -> device -> endpointID -> queue -> IO (Id MTRClusterTLSClientManagement)
initWithDevice_endpointID_queue mtrClusterTLSClientManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterTLSClientManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @provisionEndpointWithParams:expectedValues:expectedValueInterval:completion:@
provisionEndpointWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
provisionEndpointWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provisionEndpointWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @findEndpointWithParams:expectedValues:expectedValueInterval:completion:@
findEndpointWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
findEndpointWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "findEndpointWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeEndpointWithParams:expectedValues:expectedValueInterval:completion:@
removeEndpointWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeEndpointWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeEndpointWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMaxProvisionedWithParams:@
readAttributeMaxProvisionedWithParamsSelector :: Selector
readAttributeMaxProvisionedWithParamsSelector = mkSelector "readAttributeMaxProvisionedWithParams:"

-- | @Selector@ for @readAttributeProvisionedEndpointsWithParams:@
readAttributeProvisionedEndpointsWithParamsSelector :: Selector
readAttributeProvisionedEndpointsWithParamsSelector = mkSelector "readAttributeProvisionedEndpointsWithParams:"

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

