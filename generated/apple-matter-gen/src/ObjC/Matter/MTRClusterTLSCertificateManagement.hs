{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster TLS Certificate Management    This Cluster is used to manage TLS Client Certificates and to provision      TLS endpoints with enough information to facilitate subsequent connection.
--
-- Generated bindings for @MTRClusterTLSCertificateManagement@.
module ObjC.Matter.MTRClusterTLSCertificateManagement
  ( MTRClusterTLSCertificateManagement
  , IsMTRClusterTLSCertificateManagement(..)
  , provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completion
  , findRootCertificateWithParams_expectedValues_expectedValueInterval_completion
  , lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completion
  , removeRootCertificateWithParams_expectedValues_expectedValueInterval_completion
  , clientCSRWithParams_expectedValues_expectedValueInterval_completion
  , provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completion
  , findClientCertificateWithParams_expectedValues_expectedValueInterval_completion
  , lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completion
  , removeClientCertificateWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMaxRootCertificatesWithParams
  , readAttributeProvisionedRootCertificatesWithParams
  , readAttributeMaxClientCertificatesWithParams
  , readAttributeProvisionedClientCertificatesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , findRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , clientCSRWithParams_expectedValues_expectedValueInterval_completionSelector
  , provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , findClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeMaxRootCertificatesWithParamsSelector
  , readAttributeProvisionedRootCertificatesWithParamsSelector
  , readAttributeMaxClientCertificatesWithParamsSelector
  , readAttributeProvisionedClientCertificatesWithParamsSelector
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

-- | @- provisionRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSCertificateManagement (mkSelector "provisionRootCertificateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- findRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
findRootCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterFindRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
findRootCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSCertificateManagement (mkSelector "findRootCertificateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- lookupRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterLookupRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSCertificateManagement (mkSelector "lookupRootCertificateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
removeRootCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterRemoveRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeRootCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSCertificateManagement (mkSelector "removeRootCertificateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clientCSRWithParams:expectedValues:expectedValueInterval:completion:@
clientCSRWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterClientCSRParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clientCSRWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSCertificateManagement (mkSelector "clientCSRWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- provisionClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSCertificateManagement (mkSelector "provisionClientCertificateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- findClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
findClientCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterFindClientCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
findClientCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSCertificateManagement (mkSelector "findClientCertificateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- lookupClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterLookupClientCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSCertificateManagement (mkSelector "lookupClientCertificateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
removeClientCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterRemoveClientCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeClientCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTLSCertificateManagement (mkSelector "removeClientCertificateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxRootCertificatesWithParams:@
readAttributeMaxRootCertificatesWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeMaxRootCertificatesWithParams mtrClusterTLSCertificateManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSCertificateManagement (mkSelector "readAttributeMaxRootCertificatesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProvisionedRootCertificatesWithParams:@
readAttributeProvisionedRootCertificatesWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeProvisionedRootCertificatesWithParams mtrClusterTLSCertificateManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSCertificateManagement (mkSelector "readAttributeProvisionedRootCertificatesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxClientCertificatesWithParams:@
readAttributeMaxClientCertificatesWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeMaxClientCertificatesWithParams mtrClusterTLSCertificateManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSCertificateManagement (mkSelector "readAttributeMaxClientCertificatesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProvisionedClientCertificatesWithParams:@
readAttributeProvisionedClientCertificatesWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeProvisionedClientCertificatesWithParams mtrClusterTLSCertificateManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSCertificateManagement (mkSelector "readAttributeProvisionedClientCertificatesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTLSCertificateManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSCertificateManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTLSCertificateManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSCertificateManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTLSCertificateManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSCertificateManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTLSCertificateManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSCertificateManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTLSCertificateManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTLSCertificateManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement => mtrClusterTLSCertificateManagement -> IO (Id MTRClusterTLSCertificateManagement)
init_ mtrClusterTLSCertificateManagement  =
    sendMsg mtrClusterTLSCertificateManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterTLSCertificateManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTLSCertificateManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTLSCertificateManagement -> device -> endpointID -> queue -> IO (Id MTRClusterTLSCertificateManagement)
initWithDevice_endpointID_queue mtrClusterTLSCertificateManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterTLSCertificateManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @provisionRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provisionRootCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @findRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
findRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
findRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "findRootCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @lookupRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "lookupRootCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
removeRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeRootCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clientCSRWithParams:expectedValues:expectedValueInterval:completion:@
clientCSRWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
clientCSRWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "clientCSRWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @provisionClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provisionClientCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @findClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
findClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
findClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "findClientCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @lookupClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "lookupClientCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
removeClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeClientCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMaxRootCertificatesWithParams:@
readAttributeMaxRootCertificatesWithParamsSelector :: Selector
readAttributeMaxRootCertificatesWithParamsSelector = mkSelector "readAttributeMaxRootCertificatesWithParams:"

-- | @Selector@ for @readAttributeProvisionedRootCertificatesWithParams:@
readAttributeProvisionedRootCertificatesWithParamsSelector :: Selector
readAttributeProvisionedRootCertificatesWithParamsSelector = mkSelector "readAttributeProvisionedRootCertificatesWithParams:"

-- | @Selector@ for @readAttributeMaxClientCertificatesWithParams:@
readAttributeMaxClientCertificatesWithParamsSelector :: Selector
readAttributeMaxClientCertificatesWithParamsSelector = mkSelector "readAttributeMaxClientCertificatesWithParams:"

-- | @Selector@ for @readAttributeProvisionedClientCertificatesWithParams:@
readAttributeProvisionedClientCertificatesWithParamsSelector :: Selector
readAttributeProvisionedClientCertificatesWithParamsSelector = mkSelector "readAttributeProvisionedClientCertificatesWithParams:"

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

