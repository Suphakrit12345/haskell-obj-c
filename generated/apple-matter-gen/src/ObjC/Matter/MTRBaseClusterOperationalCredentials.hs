{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Operational Credentials
--
-- This cluster is used to add or remove Operational Credentials on a Commissionee or Node, as well as manage the associated Fabrics.
--
-- Generated bindings for @MTRBaseClusterOperationalCredentials@.
module ObjC.Matter.MTRBaseClusterOperationalCredentials
  ( MTRBaseClusterOperationalCredentials
  , IsMTRBaseClusterOperationalCredentials(..)
  , attestationRequestWithParams_completion
  , certificateChainRequestWithParams_completion
  , csrRequestWithParams_completion
  , addNOCWithParams_completion
  , updateNOCWithParams_completion
  , updateFabricLabelWithParams_completion
  , removeFabricWithParams_completion
  , addTrustedRootCertificateWithParams_completion
  , setVIDVerificationStatementWithParams_completion
  , setVIDVerificationStatementWithCompletion
  , signVIDVerificationRequestWithParams_completion
  , readAttributeNOCsWithParams_completion
  , subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandler
  , readAttributeNOCsWithClusterStateCache_endpoint_queue_completion
  , readAttributeFabricsWithParams_completion
  , subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandler
  , readAttributeFabricsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedFabricsWithCompletion
  , subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completion
  , readAttributeCommissionedFabricsWithCompletion
  , subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completion
  , readAttributeTrustedRootCertificatesWithCompletion
  , subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandler
  , readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentFabricIndexWithCompletion
  , subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completion
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
  , initWithDevice_endpoint_queue
  , attestationRequestWithParams_completionHandler
  , certificateChainRequestWithParams_completionHandler
  , csrRequestWithParams_completionHandler
  , addNOCWithParams_completionHandler
  , updateNOCWithParams_completionHandler
  , updateFabricLabelWithParams_completionHandler
  , removeFabricWithParams_completionHandler
  , addTrustedRootCertificateWithParams_completionHandler
  , readAttributeNOCsWithParams_completionHandler
  , subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFabricsWithParams_completionHandler
  , subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSupportedFabricsWithCompletionHandler
  , subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCommissionedFabricsWithCompletionHandler
  , subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeTrustedRootCertificatesWithCompletionHandler
  , subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentFabricIndexWithCompletionHandler
  , subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeGeneratedCommandListWithCompletionHandler
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAcceptedCommandListWithCompletionHandler
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAttributeListWithCompletionHandler
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFeatureMapWithCompletionHandler
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeClusterRevisionWithCompletionHandler
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler
  , initWithDevice_endpointID_queue
  , attestationRequestWithParams_completionSelector
  , certificateChainRequestWithParams_completionSelector
  , csrRequestWithParams_completionSelector
  , addNOCWithParams_completionSelector
  , updateNOCWithParams_completionSelector
  , updateFabricLabelWithParams_completionSelector
  , removeFabricWithParams_completionSelector
  , addTrustedRootCertificateWithParams_completionSelector
  , setVIDVerificationStatementWithParams_completionSelector
  , setVIDVerificationStatementWithCompletionSelector
  , signVIDVerificationRequestWithParams_completionSelector
  , readAttributeNOCsWithParams_completionSelector
  , subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNOCsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFabricsWithParams_completionSelector
  , subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeFabricsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedFabricsWithCompletionSelector
  , subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCommissionedFabricsWithCompletionSelector
  , subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTrustedRootCertificatesWithCompletionSelector
  , subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentFabricIndexWithCompletionSelector
  , subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completionSelector
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
  , initWithDevice_endpoint_queueSelector
  , attestationRequestWithParams_completionHandlerSelector
  , certificateChainRequestWithParams_completionHandlerSelector
  , csrRequestWithParams_completionHandlerSelector
  , addNOCWithParams_completionHandlerSelector
  , updateNOCWithParams_completionHandlerSelector
  , updateFabricLabelWithParams_completionHandlerSelector
  , removeFabricWithParams_completionHandlerSelector
  , addTrustedRootCertificateWithParams_completionHandlerSelector
  , readAttributeNOCsWithParams_completionHandlerSelector
  , subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFabricsWithParams_completionHandlerSelector
  , subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSupportedFabricsWithCompletionHandlerSelector
  , subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCommissionedFabricsWithCompletionHandlerSelector
  , subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeTrustedRootCertificatesWithCompletionHandlerSelector
  , subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentFabricIndexWithCompletionHandlerSelector
  , subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command AttestationRequest
--
-- Sender is requesting attestation information from the receiver.
--
-- ObjC selector: @- attestationRequestWithParams:completion:@
attestationRequestWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAttestationRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
attestationRequestWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "attestationRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command CertificateChainRequest
--
-- Sender is requesting a device attestation certificate from the receiver.
--
-- ObjC selector: @- certificateChainRequestWithParams:completion:@
certificateChainRequestWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterCertificateChainRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
certificateChainRequestWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "certificateChainRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command CSRRequest
--
-- Sender is requesting a certificate signing request (CSR) from the receiver.
--
-- ObjC selector: @- CSRRequestWithParams:completion:@
csrRequestWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterCSRRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
csrRequestWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "CSRRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command AddNOC
--
-- Sender is requesting to add the new node operational certificates.
--
-- ObjC selector: @- addNOCWithParams:completion:@
addNOCWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddNOCParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
addNOCWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "addNOCWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command UpdateNOC
--
-- This command SHALL replace the NOC and optional associated ICAC (if present) scoped under the accessing fabric upon successful validation of all arguments and preconditions.
--
-- ObjC selector: @- updateNOCWithParams:completion:@
updateNOCWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateNOCParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
updateNOCWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "updateNOCWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command UpdateFabricLabel
--
-- This command SHALL be used by an Administrative Node to set the user-visible Label field for a given Fabric, as reflected by entries in the Fabrics attribute.
--
-- ObjC selector: @- updateFabricLabelWithParams:completion:@
updateFabricLabelWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateFabricLabelParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
updateFabricLabelWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "updateFabricLabelWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveFabric
--
-- This command is used by Administrative Nodes to remove a given fabric index and delete all associated fabric-scoped data.
--
-- ObjC selector: @- removeFabricWithParams:completion:@
removeFabricWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterRemoveFabricParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
removeFabricWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "removeFabricWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command AddTrustedRootCertificate
--
-- This command SHALL add a Trusted Root CA Certificate, provided as its CHIP Certificate representation.
--
-- ObjC selector: @- addTrustedRootCertificateWithParams:completion:@
addTrustedRootCertificateWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
addTrustedRootCertificateWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "addTrustedRootCertificateWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SetVIDVerificationStatement
--
-- This command SHALL be used to update any of the accessing fabric's associated VendorID, VidVerificatioNStatement or VVSC (Vendor Verification Signing Certificate).
--
-- ObjC selector: @- setVIDVerificationStatementWithParams:completion:@
setVIDVerificationStatementWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
setVIDVerificationStatementWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "setVIDVerificationStatementWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setVIDVerificationStatementWithCompletion:@
setVIDVerificationStatementWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
setVIDVerificationStatementWithCompletion mtrBaseClusterOperationalCredentials  completion =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "setVIDVerificationStatementWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command SignVIDVerificationRequest
--
-- This command SHALL be used to request that the server authenticate the fabric associated with the FabricIndex given.
--
-- ObjC selector: @- signVIDVerificationRequestWithParams:completion:@
signVIDVerificationRequestWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
signVIDVerificationRequestWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "signVIDVerificationRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNOCsWithParams:completion:@
readAttributeNOCsWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRReadParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
readAttributeNOCsWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeNOCsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNOCsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeNOCsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNOCsWithClusterStateCache:endpoint:queue:completion:@
readAttributeNOCsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNOCsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNOCsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFabricsWithParams:completion:@
readAttributeFabricsWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRReadParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
readAttributeFabricsWithParams_completion mtrBaseClusterOperationalCredentials  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeFabricsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeFabricsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeFabricsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFabricsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFabricsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedFabricsWithCompletion:@
readAttributeSupportedFabricsWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeSupportedFabricsWithCompletion mtrBaseClusterOperationalCredentials  completion =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeSupportedFabricsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeSupportedFabricsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedFabricsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCommissionedFabricsWithCompletion:@
readAttributeCommissionedFabricsWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeCommissionedFabricsWithCompletion mtrBaseClusterOperationalCredentials  completion =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeCommissionedFabricsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCommissionedFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeCommissionedFabricsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCommissionedFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCommissionedFabricsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTrustedRootCertificatesWithCompletion:@
readAttributeTrustedRootCertificatesWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeTrustedRootCertificatesWithCompletion mtrBaseClusterOperationalCredentials  completion =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeTrustedRootCertificatesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTrustedRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeTrustedRootCertificatesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTrustedRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTrustedRootCertificatesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentFabricIndexWithCompletion:@
readAttributeCurrentFabricIndexWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeCurrentFabricIndexWithCompletion mtrBaseClusterOperationalCredentials  completion =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeCurrentFabricIndexWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeCurrentFabricIndexWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentFabricIndexWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterOperationalCredentials  completion =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterOperationalCredentials  completion =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterOperationalCredentials  completion =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterOperationalCredentials  completion =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterOperationalCredentials  completion =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> IO (Id MTRBaseClusterOperationalCredentials)
init_ mtrBaseClusterOperationalCredentials  =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterOperationalCredentials)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterOperationalCredentials -> device -> CUShort -> queue -> IO (Id MTRBaseClusterOperationalCredentials)
initWithDevice_endpoint_queue mtrBaseClusterOperationalCredentials  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterOperationalCredentials (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- attestationRequestWithParams:completionHandler:@
attestationRequestWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAttestationRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
attestationRequestWithParams_completionHandler mtrBaseClusterOperationalCredentials  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "attestationRequestWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- certificateChainRequestWithParams:completionHandler:@
certificateChainRequestWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterCertificateChainRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
certificateChainRequestWithParams_completionHandler mtrBaseClusterOperationalCredentials  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "certificateChainRequestWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- CSRRequestWithParams:completionHandler:@
csrRequestWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterCSRRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
csrRequestWithParams_completionHandler mtrBaseClusterOperationalCredentials  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "CSRRequestWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addNOCWithParams:completionHandler:@
addNOCWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddNOCParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
addNOCWithParams_completionHandler mtrBaseClusterOperationalCredentials  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "addNOCWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- updateNOCWithParams:completionHandler:@
updateNOCWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateNOCParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
updateNOCWithParams_completionHandler mtrBaseClusterOperationalCredentials  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "updateNOCWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- updateFabricLabelWithParams:completionHandler:@
updateFabricLabelWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateFabricLabelParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
updateFabricLabelWithParams_completionHandler mtrBaseClusterOperationalCredentials  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "updateFabricLabelWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removeFabricWithParams:completionHandler:@
removeFabricWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterRemoveFabricParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
removeFabricWithParams_completionHandler mtrBaseClusterOperationalCredentials  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "removeFabricWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addTrustedRootCertificateWithParams:completionHandler:@
addTrustedRootCertificateWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
addTrustedRootCertificateWithParams_completionHandler mtrBaseClusterOperationalCredentials  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "addTrustedRootCertificateWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeNOCsWithParams:completionHandler:@
readAttributeNOCsWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRReadParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
readAttributeNOCsWithParams_completionHandler mtrBaseClusterOperationalCredentials  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeNOCsWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeNOCsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeNOCsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNOCsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNOCsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFabricsWithParams:completionHandler:@
readAttributeFabricsWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRReadParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
readAttributeFabricsWithParams_completionHandler mtrBaseClusterOperationalCredentials  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeFabricsWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFabricsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSupportedFabricsWithCompletionHandler:@
readAttributeSupportedFabricsWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeSupportedFabricsWithCompletionHandler mtrBaseClusterOperationalCredentials  completionHandler =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeSupportedFabricsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSupportedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeSupportedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedFabricsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCommissionedFabricsWithCompletionHandler:@
readAttributeCommissionedFabricsWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeCommissionedFabricsWithCompletionHandler mtrBaseClusterOperationalCredentials  completionHandler =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeCommissionedFabricsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCommissionedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeCommissionedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCommissionedFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCommissionedFabricsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeTrustedRootCertificatesWithCompletionHandler:@
readAttributeTrustedRootCertificatesWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeTrustedRootCertificatesWithCompletionHandler mtrBaseClusterOperationalCredentials  completionHandler =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeTrustedRootCertificatesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeTrustedRootCertificatesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeTrustedRootCertificatesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTrustedRootCertificatesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTrustedRootCertificatesWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCurrentFabricIndexWithCompletionHandler:@
readAttributeCurrentFabricIndexWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeCurrentFabricIndexWithCompletionHandler mtrBaseClusterOperationalCredentials  completionHandler =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeCurrentFabricIndexWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCurrentFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeCurrentFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentFabricIndexWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentFabricIndexWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterOperationalCredentials  completionHandler =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterOperationalCredentials  completionHandler =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterOperationalCredentials  completionHandler =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterOperationalCredentials  completionHandler =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterOperationalCredentials  completionHandler =
    sendMsg mtrBaseClusterOperationalCredentials (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterOperationalCredentials -> device -> endpointID -> queue -> IO (Id MTRBaseClusterOperationalCredentials)
initWithDevice_endpointID_queue mtrBaseClusterOperationalCredentials  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterOperationalCredentials (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attestationRequestWithParams:completion:@
attestationRequestWithParams_completionSelector :: Selector
attestationRequestWithParams_completionSelector = mkSelector "attestationRequestWithParams:completion:"

-- | @Selector@ for @certificateChainRequestWithParams:completion:@
certificateChainRequestWithParams_completionSelector :: Selector
certificateChainRequestWithParams_completionSelector = mkSelector "certificateChainRequestWithParams:completion:"

-- | @Selector@ for @CSRRequestWithParams:completion:@
csrRequestWithParams_completionSelector :: Selector
csrRequestWithParams_completionSelector = mkSelector "CSRRequestWithParams:completion:"

-- | @Selector@ for @addNOCWithParams:completion:@
addNOCWithParams_completionSelector :: Selector
addNOCWithParams_completionSelector = mkSelector "addNOCWithParams:completion:"

-- | @Selector@ for @updateNOCWithParams:completion:@
updateNOCWithParams_completionSelector :: Selector
updateNOCWithParams_completionSelector = mkSelector "updateNOCWithParams:completion:"

-- | @Selector@ for @updateFabricLabelWithParams:completion:@
updateFabricLabelWithParams_completionSelector :: Selector
updateFabricLabelWithParams_completionSelector = mkSelector "updateFabricLabelWithParams:completion:"

-- | @Selector@ for @removeFabricWithParams:completion:@
removeFabricWithParams_completionSelector :: Selector
removeFabricWithParams_completionSelector = mkSelector "removeFabricWithParams:completion:"

-- | @Selector@ for @addTrustedRootCertificateWithParams:completion:@
addTrustedRootCertificateWithParams_completionSelector :: Selector
addTrustedRootCertificateWithParams_completionSelector = mkSelector "addTrustedRootCertificateWithParams:completion:"

-- | @Selector@ for @setVIDVerificationStatementWithParams:completion:@
setVIDVerificationStatementWithParams_completionSelector :: Selector
setVIDVerificationStatementWithParams_completionSelector = mkSelector "setVIDVerificationStatementWithParams:completion:"

-- | @Selector@ for @setVIDVerificationStatementWithCompletion:@
setVIDVerificationStatementWithCompletionSelector :: Selector
setVIDVerificationStatementWithCompletionSelector = mkSelector "setVIDVerificationStatementWithCompletion:"

-- | @Selector@ for @signVIDVerificationRequestWithParams:completion:@
signVIDVerificationRequestWithParams_completionSelector :: Selector
signVIDVerificationRequestWithParams_completionSelector = mkSelector "signVIDVerificationRequestWithParams:completion:"

-- | @Selector@ for @readAttributeNOCsWithParams:completion:@
readAttributeNOCsWithParams_completionSelector :: Selector
readAttributeNOCsWithParams_completionSelector = mkSelector "readAttributeNOCsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeNOCsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNOCsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNOCsWithClusterStateCache:endpoint:queue:completion:@
readAttributeNOCsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNOCsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNOCsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFabricsWithParams:completion:@
readAttributeFabricsWithParams_completionSelector :: Selector
readAttributeFabricsWithParams_completionSelector = mkSelector "readAttributeFabricsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFabricsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeFabricsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeFabricsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFabricsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedFabricsWithCompletion:@
readAttributeSupportedFabricsWithCompletionSelector :: Selector
readAttributeSupportedFabricsWithCompletionSelector = mkSelector "readAttributeSupportedFabricsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedFabricsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedFabricsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCommissionedFabricsWithCompletion:@
readAttributeCommissionedFabricsWithCompletionSelector :: Selector
readAttributeCommissionedFabricsWithCompletionSelector = mkSelector "readAttributeCommissionedFabricsWithCompletion:"

-- | @Selector@ for @subscribeAttributeCommissionedFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCommissionedFabricsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCommissionedFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCommissionedFabricsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTrustedRootCertificatesWithCompletion:@
readAttributeTrustedRootCertificatesWithCompletionSelector :: Selector
readAttributeTrustedRootCertificatesWithCompletionSelector = mkSelector "readAttributeTrustedRootCertificatesWithCompletion:"

-- | @Selector@ for @subscribeAttributeTrustedRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTrustedRootCertificatesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTrustedRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTrustedRootCertificatesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentFabricIndexWithCompletion:@
readAttributeCurrentFabricIndexWithCompletionSelector :: Selector
readAttributeCurrentFabricIndexWithCompletionSelector = mkSelector "readAttributeCurrentFabricIndexWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentFabricIndexWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentFabricIndexWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @attestationRequestWithParams:completionHandler:@
attestationRequestWithParams_completionHandlerSelector :: Selector
attestationRequestWithParams_completionHandlerSelector = mkSelector "attestationRequestWithParams:completionHandler:"

-- | @Selector@ for @certificateChainRequestWithParams:completionHandler:@
certificateChainRequestWithParams_completionHandlerSelector :: Selector
certificateChainRequestWithParams_completionHandlerSelector = mkSelector "certificateChainRequestWithParams:completionHandler:"

-- | @Selector@ for @CSRRequestWithParams:completionHandler:@
csrRequestWithParams_completionHandlerSelector :: Selector
csrRequestWithParams_completionHandlerSelector = mkSelector "CSRRequestWithParams:completionHandler:"

-- | @Selector@ for @addNOCWithParams:completionHandler:@
addNOCWithParams_completionHandlerSelector :: Selector
addNOCWithParams_completionHandlerSelector = mkSelector "addNOCWithParams:completionHandler:"

-- | @Selector@ for @updateNOCWithParams:completionHandler:@
updateNOCWithParams_completionHandlerSelector :: Selector
updateNOCWithParams_completionHandlerSelector = mkSelector "updateNOCWithParams:completionHandler:"

-- | @Selector@ for @updateFabricLabelWithParams:completionHandler:@
updateFabricLabelWithParams_completionHandlerSelector :: Selector
updateFabricLabelWithParams_completionHandlerSelector = mkSelector "updateFabricLabelWithParams:completionHandler:"

-- | @Selector@ for @removeFabricWithParams:completionHandler:@
removeFabricWithParams_completionHandlerSelector :: Selector
removeFabricWithParams_completionHandlerSelector = mkSelector "removeFabricWithParams:completionHandler:"

-- | @Selector@ for @addTrustedRootCertificateWithParams:completionHandler:@
addTrustedRootCertificateWithParams_completionHandlerSelector :: Selector
addTrustedRootCertificateWithParams_completionHandlerSelector = mkSelector "addTrustedRootCertificateWithParams:completionHandler:"

-- | @Selector@ for @readAttributeNOCsWithParams:completionHandler:@
readAttributeNOCsWithParams_completionHandlerSelector :: Selector
readAttributeNOCsWithParams_completionHandlerSelector = mkSelector "readAttributeNOCsWithParams:completionHandler:"

-- | @Selector@ for @subscribeAttributeNOCsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNOCsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNOCsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeNOCsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFabricsWithParams:completionHandler:@
readAttributeFabricsWithParams_completionHandlerSelector :: Selector
readAttributeFabricsWithParams_completionHandlerSelector = mkSelector "readAttributeFabricsWithParams:completionHandler:"

-- | @Selector@ for @subscribeAttributeFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFabricsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSupportedFabricsWithCompletionHandler:@
readAttributeSupportedFabricsWithCompletionHandlerSelector :: Selector
readAttributeSupportedFabricsWithCompletionHandlerSelector = mkSelector "readAttributeSupportedFabricsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSupportedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSupportedFabricsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCommissionedFabricsWithCompletionHandler:@
readAttributeCommissionedFabricsWithCompletionHandlerSelector :: Selector
readAttributeCommissionedFabricsWithCompletionHandlerSelector = mkSelector "readAttributeCommissionedFabricsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCommissionedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCommissionedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCommissionedFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCommissionedFabricsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeTrustedRootCertificatesWithCompletionHandler:@
readAttributeTrustedRootCertificatesWithCompletionHandlerSelector :: Selector
readAttributeTrustedRootCertificatesWithCompletionHandlerSelector = mkSelector "readAttributeTrustedRootCertificatesWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeTrustedRootCertificatesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTrustedRootCertificatesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTrustedRootCertificatesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeTrustedRootCertificatesWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentFabricIndexWithCompletionHandler:@
readAttributeCurrentFabricIndexWithCompletionHandlerSelector :: Selector
readAttributeCurrentFabricIndexWithCompletionHandlerSelector = mkSelector "readAttributeCurrentFabricIndexWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentFabricIndexWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentFabricIndexWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandlerSelector :: Selector
readAttributeGeneratedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandlerSelector :: Selector
readAttributeAcceptedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandlerSelector :: Selector
readAttributeAttributeListWithCompletionHandlerSelector = mkSelector "readAttributeAttributeListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandlerSelector :: Selector
readAttributeFeatureMapWithCompletionHandlerSelector = mkSelector "readAttributeFeatureMapWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandlerSelector :: Selector
readAttributeClusterRevisionWithCompletionHandlerSelector = mkSelector "readAttributeClusterRevisionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

