{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster TLS Certificate Management
--
-- This Cluster is used to manage TLS Client Certificates and to provision      TLS endpoints with enough information to facilitate subsequent connection.
--
-- Generated bindings for @MTRBaseClusterTLSCertificateManagement@.
module ObjC.Matter.MTRBaseClusterTLSCertificateManagement
  ( MTRBaseClusterTLSCertificateManagement
  , IsMTRBaseClusterTLSCertificateManagement(..)
  , provisionRootCertificateWithParams_completion
  , findRootCertificateWithParams_completion
  , lookupRootCertificateWithParams_completion
  , removeRootCertificateWithParams_completion
  , clientCSRWithParams_completion
  , provisionClientCertificateWithParams_completion
  , findClientCertificateWithParams_completion
  , lookupClientCertificateWithParams_completion
  , removeClientCertificateWithParams_completion
  , readAttributeMaxRootCertificatesWithCompletion
  , subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completion
  , readAttributeProvisionedRootCertificatesWithParams_completion
  , subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandler
  , readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxClientCertificatesWithCompletion
  , subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completion
  , readAttributeProvisionedClientCertificatesWithParams_completion
  , subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandler
  , readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completion
  , readAttributeGeneratedCommandListWithCompletion
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAcceptedCommandListWithCompletion
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAttributeListWithCompletion
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion
  , readAttributeFeatureMapWithCompletion
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion
  , readAttributeClusterRevisionWithCompletion
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion
  , init_
  , new
  , initWithDevice_endpointID_queue
  , provisionRootCertificateWithParams_completionSelector
  , findRootCertificateWithParams_completionSelector
  , lookupRootCertificateWithParams_completionSelector
  , removeRootCertificateWithParams_completionSelector
  , clientCSRWithParams_completionSelector
  , provisionClientCertificateWithParams_completionSelector
  , findClientCertificateWithParams_completionSelector
  , lookupClientCertificateWithParams_completionSelector
  , removeClientCertificateWithParams_completionSelector
  , readAttributeMaxRootCertificatesWithCompletionSelector
  , subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProvisionedRootCertificatesWithParams_completionSelector
  , subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxClientCertificatesWithCompletionSelector
  , subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProvisionedClientCertificatesWithParams_completionSelector
  , subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command ProvisionRootCertificate
--
-- This command SHALL provision a newly provided certificate, or rotate an existing one, based on the contents of the CAID field.
--
-- ObjC selector: @- provisionRootCertificateWithParams:completion:@
provisionRootCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
provisionRootCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "provisionRootCertificateWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command FindRootCertificate
--
-- This command SHALL return the specified TLS root certificate, or all provisioned TLS root certificates for the accessing fabric, based on the contents of the CAID field.
--
-- ObjC selector: @- findRootCertificateWithParams:completion:@
findRootCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterFindRootCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
findRootCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "findRootCertificateWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command LookupRootCertificate
--
-- This command SHALL return the CAID for the passed in fingerprint.
--
-- ObjC selector: @- lookupRootCertificateWithParams:completion:@
lookupRootCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterLookupRootCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
lookupRootCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "lookupRootCertificateWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveRootCertificate
--
-- This command SHALL be generated to request the server removes the certificate provisioned to the provided Certificate Authority ID.
--
-- ObjC selector: @- removeRootCertificateWithParams:completion:@
removeRootCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterRemoveRootCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
removeRootCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "removeRootCertificateWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ClientCSR
--
-- This command SHALL be generated to request the Node generates a certificate signing request for a new TLS key pair or use an existing CCDID for certificate rotation.
--
-- ObjC selector: @- clientCSRWithParams:completion:@
clientCSRWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterClientCSRParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
clientCSRWithParams_completion mtrBaseClusterTLSCertificateManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "clientCSRWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ProvisionClientCertificate
--
-- This command SHALL be generated to request the Node provisions newly provided Client Certificate Details, or rotate an existing client certificate.
--
-- ObjC selector: @- provisionClientCertificateWithParams:completion:@
provisionClientCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
provisionClientCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "provisionClientCertificateWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command FindClientCertificate
--
-- This command SHALL return the TLSClientCertificateDetailStruct for the passed in CCDID, or all TLS client certificates for the accessing fabric, based on the contents of the CCDID field.
--
-- ObjC selector: @- findClientCertificateWithParams:completion:@
findClientCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterFindClientCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
findClientCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "findClientCertificateWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command LookupClientCertificate
--
-- This command SHALL return the CCDID for the passed in Fingerprint.
--
-- ObjC selector: @- lookupClientCertificateWithParams:completion:@
lookupClientCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterLookupClientCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
lookupClientCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "lookupClientCertificateWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveClientCertificate
--
-- This command SHALL be used to request the Node removes all stored information for the provided CCDID.
--
-- ObjC selector: @- removeClientCertificateWithParams:completion:@
removeClientCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterRemoveClientCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
removeClientCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "removeClientCertificateWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxRootCertificatesWithCompletion:@
readAttributeMaxRootCertificatesWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeMaxRootCertificatesWithCompletion mtrBaseClusterTLSCertificateManagement  completion =
    sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "readAttributeMaxRootCertificatesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "subscribeAttributeMaxRootCertificatesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxRootCertificatesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProvisionedRootCertificatesWithParams:completion:@
readAttributeProvisionedRootCertificatesWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRReadParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
readAttributeProvisionedRootCertificatesWithParams_completion mtrBaseClusterTLSCertificateManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "readAttributeProvisionedRootCertificatesWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProvisionedRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "subscribeAttributeProvisionedRootCertificatesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProvisionedRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProvisionedRootCertificatesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxClientCertificatesWithCompletion:@
readAttributeMaxClientCertificatesWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeMaxClientCertificatesWithCompletion mtrBaseClusterTLSCertificateManagement  completion =
    sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "readAttributeMaxClientCertificatesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxClientCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "subscribeAttributeMaxClientCertificatesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxClientCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxClientCertificatesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProvisionedClientCertificatesWithParams:completion:@
readAttributeProvisionedClientCertificatesWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRReadParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
readAttributeProvisionedClientCertificatesWithParams_completion mtrBaseClusterTLSCertificateManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "readAttributeProvisionedClientCertificatesWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProvisionedClientCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "subscribeAttributeProvisionedClientCertificatesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProvisionedClientCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProvisionedClientCertificatesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTLSCertificateManagement  completion =
    sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTLSCertificateManagement  completion =
    sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTLSCertificateManagement  completion =
    sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTLSCertificateManagement  completion =
    sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTLSCertificateManagement  completion =
    sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> IO (Id MTRBaseClusterTLSCertificateManagement)
init_ mtrBaseClusterTLSCertificateManagement  =
    sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterTLSCertificateManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTLSCertificateManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTLSCertificateManagement)
initWithDevice_endpointID_queue mtrBaseClusterTLSCertificateManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterTLSCertificateManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @provisionRootCertificateWithParams:completion:@
provisionRootCertificateWithParams_completionSelector :: Selector
provisionRootCertificateWithParams_completionSelector = mkSelector "provisionRootCertificateWithParams:completion:"

-- | @Selector@ for @findRootCertificateWithParams:completion:@
findRootCertificateWithParams_completionSelector :: Selector
findRootCertificateWithParams_completionSelector = mkSelector "findRootCertificateWithParams:completion:"

-- | @Selector@ for @lookupRootCertificateWithParams:completion:@
lookupRootCertificateWithParams_completionSelector :: Selector
lookupRootCertificateWithParams_completionSelector = mkSelector "lookupRootCertificateWithParams:completion:"

-- | @Selector@ for @removeRootCertificateWithParams:completion:@
removeRootCertificateWithParams_completionSelector :: Selector
removeRootCertificateWithParams_completionSelector = mkSelector "removeRootCertificateWithParams:completion:"

-- | @Selector@ for @clientCSRWithParams:completion:@
clientCSRWithParams_completionSelector :: Selector
clientCSRWithParams_completionSelector = mkSelector "clientCSRWithParams:completion:"

-- | @Selector@ for @provisionClientCertificateWithParams:completion:@
provisionClientCertificateWithParams_completionSelector :: Selector
provisionClientCertificateWithParams_completionSelector = mkSelector "provisionClientCertificateWithParams:completion:"

-- | @Selector@ for @findClientCertificateWithParams:completion:@
findClientCertificateWithParams_completionSelector :: Selector
findClientCertificateWithParams_completionSelector = mkSelector "findClientCertificateWithParams:completion:"

-- | @Selector@ for @lookupClientCertificateWithParams:completion:@
lookupClientCertificateWithParams_completionSelector :: Selector
lookupClientCertificateWithParams_completionSelector = mkSelector "lookupClientCertificateWithParams:completion:"

-- | @Selector@ for @removeClientCertificateWithParams:completion:@
removeClientCertificateWithParams_completionSelector :: Selector
removeClientCertificateWithParams_completionSelector = mkSelector "removeClientCertificateWithParams:completion:"

-- | @Selector@ for @readAttributeMaxRootCertificatesWithCompletion:@
readAttributeMaxRootCertificatesWithCompletionSelector :: Selector
readAttributeMaxRootCertificatesWithCompletionSelector = mkSelector "readAttributeMaxRootCertificatesWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxRootCertificatesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxRootCertificatesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProvisionedRootCertificatesWithParams:completion:@
readAttributeProvisionedRootCertificatesWithParams_completionSelector :: Selector
readAttributeProvisionedRootCertificatesWithParams_completionSelector = mkSelector "readAttributeProvisionedRootCertificatesWithParams:completion:"

-- | @Selector@ for @subscribeAttributeProvisionedRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProvisionedRootCertificatesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProvisionedRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProvisionedRootCertificatesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxClientCertificatesWithCompletion:@
readAttributeMaxClientCertificatesWithCompletionSelector :: Selector
readAttributeMaxClientCertificatesWithCompletionSelector = mkSelector "readAttributeMaxClientCertificatesWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxClientCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxClientCertificatesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxClientCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxClientCertificatesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProvisionedClientCertificatesWithParams:completion:@
readAttributeProvisionedClientCertificatesWithParams_completionSelector :: Selector
readAttributeProvisionedClientCertificatesWithParams_completionSelector = mkSelector "readAttributeProvisionedClientCertificatesWithParams:completion:"

-- | @Selector@ for @subscribeAttributeProvisionedClientCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProvisionedClientCertificatesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProvisionedClientCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProvisionedClientCertificatesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletionSelector :: Selector
readAttributeGeneratedCommandListWithCompletionSelector = mkSelector "readAttributeGeneratedCommandListWithCompletion:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletionSelector :: Selector
readAttributeAcceptedCommandListWithCompletionSelector = mkSelector "readAttributeAcceptedCommandListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletionSelector :: Selector
readAttributeAttributeListWithCompletionSelector = mkSelector "readAttributeAttributeListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletionSelector :: Selector
readAttributeFeatureMapWithCompletionSelector = mkSelector "readAttributeFeatureMapWithCompletion:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletionSelector :: Selector
readAttributeClusterRevisionWithCompletionSelector = mkSelector "readAttributeClusterRevisionWithCompletion:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

