{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster General Commissioning
--
-- This cluster is used to manage global aspects of the Commissioning flow.
--
-- Generated bindings for @MTRBaseClusterGeneralCommissioning@.
module ObjC.Matter.MTRBaseClusterGeneralCommissioning
  ( MTRBaseClusterGeneralCommissioning
  , IsMTRBaseClusterGeneralCommissioning(..)
  , armFailSafeWithParams_completion
  , setRegulatoryConfigWithParams_completion
  , commissioningCompleteWithParams_completion
  , commissioningCompleteWithCompletion
  , setTCAcknowledgementsWithParams_completion
  , readAttributeBreadcrumbWithCompletion
  , writeAttributeBreadcrumbWithValue_completion
  , writeAttributeBreadcrumbWithValue_params_completion
  , subscribeAttributeBreadcrumbWithParams_subscriptionEstablished_reportHandler
  , readAttributeBreadcrumbWithClusterStateCache_endpoint_queue_completion
  , readAttributeBasicCommissioningInfoWithCompletion
  , subscribeAttributeBasicCommissioningInfoWithParams_subscriptionEstablished_reportHandler
  , readAttributeBasicCommissioningInfoWithClusterStateCache_endpoint_queue_completion
  , readAttributeRegulatoryConfigWithCompletion
  , subscribeAttributeRegulatoryConfigWithParams_subscriptionEstablished_reportHandler
  , readAttributeRegulatoryConfigWithClusterStateCache_endpoint_queue_completion
  , readAttributeLocationCapabilityWithCompletion
  , subscribeAttributeLocationCapabilityWithParams_subscriptionEstablished_reportHandler
  , readAttributeLocationCapabilityWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportsConcurrentConnectionWithCompletion
  , subscribeAttributeSupportsConcurrentConnectionWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportsConcurrentConnectionWithClusterStateCache_endpoint_queue_completion
  , readAttributeTCAcceptedVersionWithCompletion
  , subscribeAttributeTCAcceptedVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeTCAcceptedVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeTCMinRequiredVersionWithCompletion
  , subscribeAttributeTCMinRequiredVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeTCMinRequiredVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeTCAcknowledgementsWithCompletion
  , subscribeAttributeTCAcknowledgementsWithParams_subscriptionEstablished_reportHandler
  , readAttributeTCAcknowledgementsWithClusterStateCache_endpoint_queue_completion
  , readAttributeTCAcknowledgementsRequiredWithCompletion
  , subscribeAttributeTCAcknowledgementsRequiredWithParams_subscriptionEstablished_reportHandler
  , readAttributeTCAcknowledgementsRequiredWithClusterStateCache_endpoint_queue_completion
  , readAttributeTCUpdateDeadlineWithCompletion
  , subscribeAttributeTCUpdateDeadlineWithParams_subscriptionEstablished_reportHandler
  , readAttributeTCUpdateDeadlineWithClusterStateCache_endpoint_queue_completion
  , readAttributeRecoveryIdentifierWithCompletion
  , subscribeAttributeRecoveryIdentifierWithParams_subscriptionEstablished_reportHandler
  , readAttributeRecoveryIdentifierWithClusterStateCache_endpoint_queue_completion
  , readAttributeNetworkRecoveryReasonWithCompletion
  , subscribeAttributeNetworkRecoveryReasonWithParams_subscriptionEstablished_reportHandler
  , readAttributeNetworkRecoveryReasonWithClusterStateCache_endpoint_queue_completion
  , readAttributeIsCommissioningWithoutPowerWithCompletion
  , subscribeAttributeIsCommissioningWithoutPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeIsCommissioningWithoutPowerWithClusterStateCache_endpoint_queue_completion
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
  , armFailSafeWithParams_completionHandler
  , setRegulatoryConfigWithParams_completionHandler
  , commissioningCompleteWithParams_completionHandler
  , commissioningCompleteWithCompletionHandler
  , readAttributeBreadcrumbWithCompletionHandler
  , writeAttributeBreadcrumbWithValue_completionHandler
  , writeAttributeBreadcrumbWithValue_params_completionHandler
  , subscribeAttributeBreadcrumbWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBreadcrumbWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBasicCommissioningInfoWithCompletionHandler
  , subscribeAttributeBasicCommissioningInfoWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBasicCommissioningInfoWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeRegulatoryConfigWithCompletionHandler
  , subscribeAttributeRegulatoryConfigWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeRegulatoryConfigWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLocationCapabilityWithCompletionHandler
  , subscribeAttributeLocationCapabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLocationCapabilityWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSupportsConcurrentConnectionWithCompletionHandler
  , subscribeAttributeSupportsConcurrentConnectionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSupportsConcurrentConnectionWithAttributeCache_endpoint_queue_completionHandler
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
  , armFailSafeWithParams_completionSelector
  , setRegulatoryConfigWithParams_completionSelector
  , commissioningCompleteWithParams_completionSelector
  , commissioningCompleteWithCompletionSelector
  , setTCAcknowledgementsWithParams_completionSelector
  , readAttributeBreadcrumbWithCompletionSelector
  , writeAttributeBreadcrumbWithValue_completionSelector
  , writeAttributeBreadcrumbWithValue_params_completionSelector
  , subscribeAttributeBreadcrumbWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBreadcrumbWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBasicCommissioningInfoWithCompletionSelector
  , subscribeAttributeBasicCommissioningInfoWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBasicCommissioningInfoWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRegulatoryConfigWithCompletionSelector
  , subscribeAttributeRegulatoryConfigWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRegulatoryConfigWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLocationCapabilityWithCompletionSelector
  , subscribeAttributeLocationCapabilityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLocationCapabilityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportsConcurrentConnectionWithCompletionSelector
  , subscribeAttributeSupportsConcurrentConnectionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportsConcurrentConnectionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTCAcceptedVersionWithCompletionSelector
  , subscribeAttributeTCAcceptedVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTCAcceptedVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTCMinRequiredVersionWithCompletionSelector
  , subscribeAttributeTCMinRequiredVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTCMinRequiredVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTCAcknowledgementsWithCompletionSelector
  , subscribeAttributeTCAcknowledgementsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTCAcknowledgementsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTCAcknowledgementsRequiredWithCompletionSelector
  , subscribeAttributeTCAcknowledgementsRequiredWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTCAcknowledgementsRequiredWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTCUpdateDeadlineWithCompletionSelector
  , subscribeAttributeTCUpdateDeadlineWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTCUpdateDeadlineWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRecoveryIdentifierWithCompletionSelector
  , subscribeAttributeRecoveryIdentifierWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRecoveryIdentifierWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNetworkRecoveryReasonWithCompletionSelector
  , subscribeAttributeNetworkRecoveryReasonWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNetworkRecoveryReasonWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeIsCommissioningWithoutPowerWithCompletionSelector
  , subscribeAttributeIsCommissioningWithoutPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeIsCommissioningWithoutPowerWithClusterStateCache_endpoint_queue_completionSelector
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
  , armFailSafeWithParams_completionHandlerSelector
  , setRegulatoryConfigWithParams_completionHandlerSelector
  , commissioningCompleteWithParams_completionHandlerSelector
  , commissioningCompleteWithCompletionHandlerSelector
  , readAttributeBreadcrumbWithCompletionHandlerSelector
  , writeAttributeBreadcrumbWithValue_completionHandlerSelector
  , writeAttributeBreadcrumbWithValue_params_completionHandlerSelector
  , subscribeAttributeBreadcrumbWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBreadcrumbWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBasicCommissioningInfoWithCompletionHandlerSelector
  , subscribeAttributeBasicCommissioningInfoWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBasicCommissioningInfoWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeRegulatoryConfigWithCompletionHandlerSelector
  , subscribeAttributeRegulatoryConfigWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeRegulatoryConfigWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLocationCapabilityWithCompletionHandlerSelector
  , subscribeAttributeLocationCapabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLocationCapabilityWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSupportsConcurrentConnectionWithCompletionHandlerSelector
  , subscribeAttributeSupportsConcurrentConnectionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportsConcurrentConnectionWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command ArmFailSafe
--
-- This command is used to arm or disarm the fail-safe timer.
--
-- ObjC selector: @- armFailSafeWithParams:completion:@
armFailSafeWithParams_completion :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterArmFailSafeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> IO ()
armFailSafeWithParams_completion mtrBaseClusterGeneralCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "armFailSafeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SetRegulatoryConfig
--
-- This command is used to set the regulatory configuration for the device.
--
-- ObjC selector: @- setRegulatoryConfigWithParams:completion:@
setRegulatoryConfigWithParams_completion :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> IO ()
setRegulatoryConfigWithParams_completion mtrBaseClusterGeneralCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "setRegulatoryConfigWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command CommissioningComplete
--
-- This command is used to indicate that the commissioning process is complete.
--
-- ObjC selector: @- commissioningCompleteWithParams:completion:@
commissioningCompleteWithParams_completion :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterCommissioningCompleteParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> IO ()
commissioningCompleteWithParams_completion mtrBaseClusterGeneralCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "commissioningCompleteWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- commissioningCompleteWithCompletion:@
commissioningCompleteWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
commissioningCompleteWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "commissioningCompleteWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command SetTCAcknowledgements
--
-- This command is used to set the user acknowledgements received in the Enhanced Setup Flow Terms & Conditions into the node.
--
-- ObjC selector: @- setTCAcknowledgementsWithParams:completion:@
setTCAcknowledgementsWithParams_completion :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> IO ()
setTCAcknowledgementsWithParams_completion mtrBaseClusterGeneralCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "setTCAcknowledgementsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBreadcrumbWithCompletion:@
readAttributeBreadcrumbWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeBreadcrumbWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeBreadcrumbWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBreadcrumbWithValue:completion:@
writeAttributeBreadcrumbWithValue_completion :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber value) => mtrBaseClusterGeneralCommissioning -> value -> Ptr () -> IO ()
writeAttributeBreadcrumbWithValue_completion mtrBaseClusterGeneralCommissioning  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "writeAttributeBreadcrumbWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBreadcrumbWithValue:params:completion:@
writeAttributeBreadcrumbWithValue_params_completion :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterGeneralCommissioning -> value -> params -> Ptr () -> IO ()
writeAttributeBreadcrumbWithValue_params_completion mtrBaseClusterGeneralCommissioning  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "writeAttributeBreadcrumbWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBreadcrumbWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBreadcrumbWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBreadcrumbWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeBreadcrumbWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBreadcrumbWithClusterStateCache:endpoint:queue:completion:@
readAttributeBreadcrumbWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBreadcrumbWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBreadcrumbWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBasicCommissioningInfoWithCompletion:@
readAttributeBasicCommissioningInfoWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeBasicCommissioningInfoWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeBasicCommissioningInfoWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBasicCommissioningInfoWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBasicCommissioningInfoWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBasicCommissioningInfoWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeBasicCommissioningInfoWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBasicCommissioningInfoWithClusterStateCache:endpoint:queue:completion:@
readAttributeBasicCommissioningInfoWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBasicCommissioningInfoWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBasicCommissioningInfoWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRegulatoryConfigWithCompletion:@
readAttributeRegulatoryConfigWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeRegulatoryConfigWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeRegulatoryConfigWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRegulatoryConfigWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRegulatoryConfigWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRegulatoryConfigWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeRegulatoryConfigWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRegulatoryConfigWithClusterStateCache:endpoint:queue:completion:@
readAttributeRegulatoryConfigWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRegulatoryConfigWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRegulatoryConfigWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLocationCapabilityWithCompletion:@
readAttributeLocationCapabilityWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeLocationCapabilityWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeLocationCapabilityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLocationCapabilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationCapabilityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocationCapabilityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeLocationCapabilityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLocationCapabilityWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocationCapabilityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocationCapabilityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLocationCapabilityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportsConcurrentConnectionWithCompletion:@
readAttributeSupportsConcurrentConnectionWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeSupportsConcurrentConnectionWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeSupportsConcurrentConnectionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportsConcurrentConnectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsConcurrentConnectionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportsConcurrentConnectionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeSupportsConcurrentConnectionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportsConcurrentConnectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportsConcurrentConnectionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportsConcurrentConnectionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportsConcurrentConnectionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTCAcceptedVersionWithCompletion:@
readAttributeTCAcceptedVersionWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeTCAcceptedVersionWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeTCAcceptedVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTCAcceptedVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTCAcceptedVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTCAcceptedVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeTCAcceptedVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTCAcceptedVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeTCAcceptedVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTCAcceptedVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTCAcceptedVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTCMinRequiredVersionWithCompletion:@
readAttributeTCMinRequiredVersionWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeTCMinRequiredVersionWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeTCMinRequiredVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTCMinRequiredVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTCMinRequiredVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTCMinRequiredVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeTCMinRequiredVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTCMinRequiredVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeTCMinRequiredVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTCMinRequiredVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTCMinRequiredVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTCAcknowledgementsWithCompletion:@
readAttributeTCAcknowledgementsWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeTCAcknowledgementsWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeTCAcknowledgementsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTCAcknowledgementsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTCAcknowledgementsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTCAcknowledgementsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeTCAcknowledgementsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTCAcknowledgementsWithClusterStateCache:endpoint:queue:completion:@
readAttributeTCAcknowledgementsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTCAcknowledgementsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTCAcknowledgementsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTCAcknowledgementsRequiredWithCompletion:@
readAttributeTCAcknowledgementsRequiredWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeTCAcknowledgementsRequiredWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeTCAcknowledgementsRequiredWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTCAcknowledgementsRequiredWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTCAcknowledgementsRequiredWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTCAcknowledgementsRequiredWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeTCAcknowledgementsRequiredWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTCAcknowledgementsRequiredWithClusterStateCache:endpoint:queue:completion:@
readAttributeTCAcknowledgementsRequiredWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTCAcknowledgementsRequiredWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTCAcknowledgementsRequiredWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTCUpdateDeadlineWithCompletion:@
readAttributeTCUpdateDeadlineWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeTCUpdateDeadlineWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeTCUpdateDeadlineWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTCUpdateDeadlineWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTCUpdateDeadlineWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTCUpdateDeadlineWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeTCUpdateDeadlineWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTCUpdateDeadlineWithClusterStateCache:endpoint:queue:completion:@
readAttributeTCUpdateDeadlineWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTCUpdateDeadlineWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTCUpdateDeadlineWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRecoveryIdentifierWithCompletion:@
readAttributeRecoveryIdentifierWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeRecoveryIdentifierWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeRecoveryIdentifierWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRecoveryIdentifierWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRecoveryIdentifierWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRecoveryIdentifierWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeRecoveryIdentifierWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRecoveryIdentifierWithClusterStateCache:endpoint:queue:completion:@
readAttributeRecoveryIdentifierWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRecoveryIdentifierWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRecoveryIdentifierWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNetworkRecoveryReasonWithCompletion:@
readAttributeNetworkRecoveryReasonWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeNetworkRecoveryReasonWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeNetworkRecoveryReasonWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNetworkRecoveryReasonWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNetworkRecoveryReasonWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNetworkRecoveryReasonWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeNetworkRecoveryReasonWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNetworkRecoveryReasonWithClusterStateCache:endpoint:queue:completion:@
readAttributeNetworkRecoveryReasonWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNetworkRecoveryReasonWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNetworkRecoveryReasonWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeIsCommissioningWithoutPowerWithCompletion:@
readAttributeIsCommissioningWithoutPowerWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeIsCommissioningWithoutPowerWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeIsCommissioningWithoutPowerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeIsCommissioningWithoutPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIsCommissioningWithoutPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeIsCommissioningWithoutPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeIsCommissioningWithoutPowerWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeIsCommissioningWithoutPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeIsCommissioningWithoutPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeIsCommissioningWithoutPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeIsCommissioningWithoutPowerWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterGeneralCommissioning  completion =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> IO (Id MTRBaseClusterGeneralCommissioning)
init_ mtrBaseClusterGeneralCommissioning  =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterGeneralCommissioning)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterGeneralCommissioning -> device -> CUShort -> queue -> IO (Id MTRBaseClusterGeneralCommissioning)
initWithDevice_endpoint_queue mtrBaseClusterGeneralCommissioning  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- armFailSafeWithParams:completionHandler:@
armFailSafeWithParams_completionHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterArmFailSafeParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> IO ()
armFailSafeWithParams_completionHandler mtrBaseClusterGeneralCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "armFailSafeWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setRegulatoryConfigWithParams:completionHandler:@
setRegulatoryConfigWithParams_completionHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> IO ()
setRegulatoryConfigWithParams_completionHandler mtrBaseClusterGeneralCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "setRegulatoryConfigWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- commissioningCompleteWithParams:completionHandler:@
commissioningCompleteWithParams_completionHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterCommissioningCompleteParams params) => mtrBaseClusterGeneralCommissioning -> params -> Ptr () -> IO ()
commissioningCompleteWithParams_completionHandler mtrBaseClusterGeneralCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "commissioningCompleteWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- commissioningCompleteWithCompletionHandler:@
commissioningCompleteWithCompletionHandler :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
commissioningCompleteWithCompletionHandler mtrBaseClusterGeneralCommissioning  completionHandler =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "commissioningCompleteWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBreadcrumbWithCompletionHandler:@
readAttributeBreadcrumbWithCompletionHandler :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeBreadcrumbWithCompletionHandler mtrBaseClusterGeneralCommissioning  completionHandler =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeBreadcrumbWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBreadcrumbWithValue:completionHandler:@
writeAttributeBreadcrumbWithValue_completionHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber value) => mtrBaseClusterGeneralCommissioning -> value -> Ptr () -> IO ()
writeAttributeBreadcrumbWithValue_completionHandler mtrBaseClusterGeneralCommissioning  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "writeAttributeBreadcrumbWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBreadcrumbWithValue:params:completionHandler:@
writeAttributeBreadcrumbWithValue_params_completionHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterGeneralCommissioning -> value -> params -> Ptr () -> IO ()
writeAttributeBreadcrumbWithValue_params_completionHandler mtrBaseClusterGeneralCommissioning  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "writeAttributeBreadcrumbWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBreadcrumbWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBreadcrumbWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBreadcrumbWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeBreadcrumbWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBreadcrumbWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBreadcrumbWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBreadcrumbWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBreadcrumbWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBasicCommissioningInfoWithCompletionHandler:@
readAttributeBasicCommissioningInfoWithCompletionHandler :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeBasicCommissioningInfoWithCompletionHandler mtrBaseClusterGeneralCommissioning  completionHandler =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeBasicCommissioningInfoWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBasicCommissioningInfoWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBasicCommissioningInfoWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBasicCommissioningInfoWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeBasicCommissioningInfoWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBasicCommissioningInfoWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBasicCommissioningInfoWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBasicCommissioningInfoWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBasicCommissioningInfoWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeRegulatoryConfigWithCompletionHandler:@
readAttributeRegulatoryConfigWithCompletionHandler :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeRegulatoryConfigWithCompletionHandler mtrBaseClusterGeneralCommissioning  completionHandler =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeRegulatoryConfigWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeRegulatoryConfigWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRegulatoryConfigWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRegulatoryConfigWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeRegulatoryConfigWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRegulatoryConfigWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRegulatoryConfigWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRegulatoryConfigWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRegulatoryConfigWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLocationCapabilityWithCompletionHandler:@
readAttributeLocationCapabilityWithCompletionHandler :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeLocationCapabilityWithCompletionHandler mtrBaseClusterGeneralCommissioning  completionHandler =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeLocationCapabilityWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLocationCapabilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationCapabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocationCapabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeLocationCapabilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLocationCapabilityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLocationCapabilityWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocationCapabilityWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLocationCapabilityWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSupportsConcurrentConnectionWithCompletionHandler:@
readAttributeSupportsConcurrentConnectionWithCompletionHandler :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeSupportsConcurrentConnectionWithCompletionHandler mtrBaseClusterGeneralCommissioning  completionHandler =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeSupportsConcurrentConnectionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSupportsConcurrentConnectionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsConcurrentConnectionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportsConcurrentConnectionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeSupportsConcurrentConnectionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportsConcurrentConnectionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportsConcurrentConnectionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportsConcurrentConnectionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportsConcurrentConnectionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterGeneralCommissioning  completionHandler =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterGeneralCommissioning  completionHandler =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterGeneralCommissioning  completionHandler =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterGeneralCommissioning  completionHandler =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning => mtrBaseClusterGeneralCommissioning -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterGeneralCommissioning  completionHandler =
    sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterGeneralCommissioning mtrBaseClusterGeneralCommissioning, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterGeneralCommissioning -> device -> endpointID -> queue -> IO (Id MTRBaseClusterGeneralCommissioning)
initWithDevice_endpointID_queue mtrBaseClusterGeneralCommissioning  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterGeneralCommissioning (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @armFailSafeWithParams:completion:@
armFailSafeWithParams_completionSelector :: Selector
armFailSafeWithParams_completionSelector = mkSelector "armFailSafeWithParams:completion:"

-- | @Selector@ for @setRegulatoryConfigWithParams:completion:@
setRegulatoryConfigWithParams_completionSelector :: Selector
setRegulatoryConfigWithParams_completionSelector = mkSelector "setRegulatoryConfigWithParams:completion:"

-- | @Selector@ for @commissioningCompleteWithParams:completion:@
commissioningCompleteWithParams_completionSelector :: Selector
commissioningCompleteWithParams_completionSelector = mkSelector "commissioningCompleteWithParams:completion:"

-- | @Selector@ for @commissioningCompleteWithCompletion:@
commissioningCompleteWithCompletionSelector :: Selector
commissioningCompleteWithCompletionSelector = mkSelector "commissioningCompleteWithCompletion:"

-- | @Selector@ for @setTCAcknowledgementsWithParams:completion:@
setTCAcknowledgementsWithParams_completionSelector :: Selector
setTCAcknowledgementsWithParams_completionSelector = mkSelector "setTCAcknowledgementsWithParams:completion:"

-- | @Selector@ for @readAttributeBreadcrumbWithCompletion:@
readAttributeBreadcrumbWithCompletionSelector :: Selector
readAttributeBreadcrumbWithCompletionSelector = mkSelector "readAttributeBreadcrumbWithCompletion:"

-- | @Selector@ for @writeAttributeBreadcrumbWithValue:completion:@
writeAttributeBreadcrumbWithValue_completionSelector :: Selector
writeAttributeBreadcrumbWithValue_completionSelector = mkSelector "writeAttributeBreadcrumbWithValue:completion:"

-- | @Selector@ for @writeAttributeBreadcrumbWithValue:params:completion:@
writeAttributeBreadcrumbWithValue_params_completionSelector :: Selector
writeAttributeBreadcrumbWithValue_params_completionSelector = mkSelector "writeAttributeBreadcrumbWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeBreadcrumbWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBreadcrumbWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBreadcrumbWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBreadcrumbWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBreadcrumbWithClusterStateCache:endpoint:queue:completion:@
readAttributeBreadcrumbWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBreadcrumbWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBreadcrumbWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBasicCommissioningInfoWithCompletion:@
readAttributeBasicCommissioningInfoWithCompletionSelector :: Selector
readAttributeBasicCommissioningInfoWithCompletionSelector = mkSelector "readAttributeBasicCommissioningInfoWithCompletion:"

-- | @Selector@ for @subscribeAttributeBasicCommissioningInfoWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBasicCommissioningInfoWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBasicCommissioningInfoWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBasicCommissioningInfoWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBasicCommissioningInfoWithClusterStateCache:endpoint:queue:completion:@
readAttributeBasicCommissioningInfoWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBasicCommissioningInfoWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBasicCommissioningInfoWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRegulatoryConfigWithCompletion:@
readAttributeRegulatoryConfigWithCompletionSelector :: Selector
readAttributeRegulatoryConfigWithCompletionSelector = mkSelector "readAttributeRegulatoryConfigWithCompletion:"

-- | @Selector@ for @subscribeAttributeRegulatoryConfigWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRegulatoryConfigWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRegulatoryConfigWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRegulatoryConfigWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRegulatoryConfigWithClusterStateCache:endpoint:queue:completion:@
readAttributeRegulatoryConfigWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRegulatoryConfigWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRegulatoryConfigWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLocationCapabilityWithCompletion:@
readAttributeLocationCapabilityWithCompletionSelector :: Selector
readAttributeLocationCapabilityWithCompletionSelector = mkSelector "readAttributeLocationCapabilityWithCompletion:"

-- | @Selector@ for @subscribeAttributeLocationCapabilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationCapabilityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLocationCapabilityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocationCapabilityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocationCapabilityWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocationCapabilityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLocationCapabilityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLocationCapabilityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportsConcurrentConnectionWithCompletion:@
readAttributeSupportsConcurrentConnectionWithCompletionSelector :: Selector
readAttributeSupportsConcurrentConnectionWithCompletionSelector = mkSelector "readAttributeSupportsConcurrentConnectionWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportsConcurrentConnectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsConcurrentConnectionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportsConcurrentConnectionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportsConcurrentConnectionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportsConcurrentConnectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportsConcurrentConnectionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportsConcurrentConnectionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportsConcurrentConnectionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTCAcceptedVersionWithCompletion:@
readAttributeTCAcceptedVersionWithCompletionSelector :: Selector
readAttributeTCAcceptedVersionWithCompletionSelector = mkSelector "readAttributeTCAcceptedVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeTCAcceptedVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTCAcceptedVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTCAcceptedVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTCAcceptedVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTCAcceptedVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeTCAcceptedVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTCAcceptedVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTCAcceptedVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTCMinRequiredVersionWithCompletion:@
readAttributeTCMinRequiredVersionWithCompletionSelector :: Selector
readAttributeTCMinRequiredVersionWithCompletionSelector = mkSelector "readAttributeTCMinRequiredVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeTCMinRequiredVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTCMinRequiredVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTCMinRequiredVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTCMinRequiredVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTCMinRequiredVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeTCMinRequiredVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTCMinRequiredVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTCMinRequiredVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTCAcknowledgementsWithCompletion:@
readAttributeTCAcknowledgementsWithCompletionSelector :: Selector
readAttributeTCAcknowledgementsWithCompletionSelector = mkSelector "readAttributeTCAcknowledgementsWithCompletion:"

-- | @Selector@ for @subscribeAttributeTCAcknowledgementsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTCAcknowledgementsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTCAcknowledgementsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTCAcknowledgementsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTCAcknowledgementsWithClusterStateCache:endpoint:queue:completion:@
readAttributeTCAcknowledgementsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTCAcknowledgementsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTCAcknowledgementsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTCAcknowledgementsRequiredWithCompletion:@
readAttributeTCAcknowledgementsRequiredWithCompletionSelector :: Selector
readAttributeTCAcknowledgementsRequiredWithCompletionSelector = mkSelector "readAttributeTCAcknowledgementsRequiredWithCompletion:"

-- | @Selector@ for @subscribeAttributeTCAcknowledgementsRequiredWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTCAcknowledgementsRequiredWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTCAcknowledgementsRequiredWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTCAcknowledgementsRequiredWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTCAcknowledgementsRequiredWithClusterStateCache:endpoint:queue:completion:@
readAttributeTCAcknowledgementsRequiredWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTCAcknowledgementsRequiredWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTCAcknowledgementsRequiredWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTCUpdateDeadlineWithCompletion:@
readAttributeTCUpdateDeadlineWithCompletionSelector :: Selector
readAttributeTCUpdateDeadlineWithCompletionSelector = mkSelector "readAttributeTCUpdateDeadlineWithCompletion:"

-- | @Selector@ for @subscribeAttributeTCUpdateDeadlineWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTCUpdateDeadlineWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTCUpdateDeadlineWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTCUpdateDeadlineWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTCUpdateDeadlineWithClusterStateCache:endpoint:queue:completion:@
readAttributeTCUpdateDeadlineWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTCUpdateDeadlineWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTCUpdateDeadlineWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRecoveryIdentifierWithCompletion:@
readAttributeRecoveryIdentifierWithCompletionSelector :: Selector
readAttributeRecoveryIdentifierWithCompletionSelector = mkSelector "readAttributeRecoveryIdentifierWithCompletion:"

-- | @Selector@ for @subscribeAttributeRecoveryIdentifierWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRecoveryIdentifierWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRecoveryIdentifierWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRecoveryIdentifierWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRecoveryIdentifierWithClusterStateCache:endpoint:queue:completion:@
readAttributeRecoveryIdentifierWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRecoveryIdentifierWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRecoveryIdentifierWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNetworkRecoveryReasonWithCompletion:@
readAttributeNetworkRecoveryReasonWithCompletionSelector :: Selector
readAttributeNetworkRecoveryReasonWithCompletionSelector = mkSelector "readAttributeNetworkRecoveryReasonWithCompletion:"

-- | @Selector@ for @subscribeAttributeNetworkRecoveryReasonWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNetworkRecoveryReasonWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNetworkRecoveryReasonWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNetworkRecoveryReasonWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNetworkRecoveryReasonWithClusterStateCache:endpoint:queue:completion:@
readAttributeNetworkRecoveryReasonWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNetworkRecoveryReasonWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNetworkRecoveryReasonWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeIsCommissioningWithoutPowerWithCompletion:@
readAttributeIsCommissioningWithoutPowerWithCompletionSelector :: Selector
readAttributeIsCommissioningWithoutPowerWithCompletionSelector = mkSelector "readAttributeIsCommissioningWithoutPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeIsCommissioningWithoutPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIsCommissioningWithoutPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeIsCommissioningWithoutPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeIsCommissioningWithoutPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeIsCommissioningWithoutPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeIsCommissioningWithoutPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeIsCommissioningWithoutPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeIsCommissioningWithoutPowerWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @armFailSafeWithParams:completionHandler:@
armFailSafeWithParams_completionHandlerSelector :: Selector
armFailSafeWithParams_completionHandlerSelector = mkSelector "armFailSafeWithParams:completionHandler:"

-- | @Selector@ for @setRegulatoryConfigWithParams:completionHandler:@
setRegulatoryConfigWithParams_completionHandlerSelector :: Selector
setRegulatoryConfigWithParams_completionHandlerSelector = mkSelector "setRegulatoryConfigWithParams:completionHandler:"

-- | @Selector@ for @commissioningCompleteWithParams:completionHandler:@
commissioningCompleteWithParams_completionHandlerSelector :: Selector
commissioningCompleteWithParams_completionHandlerSelector = mkSelector "commissioningCompleteWithParams:completionHandler:"

-- | @Selector@ for @commissioningCompleteWithCompletionHandler:@
commissioningCompleteWithCompletionHandlerSelector :: Selector
commissioningCompleteWithCompletionHandlerSelector = mkSelector "commissioningCompleteWithCompletionHandler:"

-- | @Selector@ for @readAttributeBreadcrumbWithCompletionHandler:@
readAttributeBreadcrumbWithCompletionHandlerSelector :: Selector
readAttributeBreadcrumbWithCompletionHandlerSelector = mkSelector "readAttributeBreadcrumbWithCompletionHandler:"

-- | @Selector@ for @writeAttributeBreadcrumbWithValue:completionHandler:@
writeAttributeBreadcrumbWithValue_completionHandlerSelector :: Selector
writeAttributeBreadcrumbWithValue_completionHandlerSelector = mkSelector "writeAttributeBreadcrumbWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeBreadcrumbWithValue:params:completionHandler:@
writeAttributeBreadcrumbWithValue_params_completionHandlerSelector :: Selector
writeAttributeBreadcrumbWithValue_params_completionHandlerSelector = mkSelector "writeAttributeBreadcrumbWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeBreadcrumbWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBreadcrumbWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBreadcrumbWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBreadcrumbWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBreadcrumbWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBreadcrumbWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBreadcrumbWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBreadcrumbWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBasicCommissioningInfoWithCompletionHandler:@
readAttributeBasicCommissioningInfoWithCompletionHandlerSelector :: Selector
readAttributeBasicCommissioningInfoWithCompletionHandlerSelector = mkSelector "readAttributeBasicCommissioningInfoWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeBasicCommissioningInfoWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBasicCommissioningInfoWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBasicCommissioningInfoWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBasicCommissioningInfoWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBasicCommissioningInfoWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBasicCommissioningInfoWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBasicCommissioningInfoWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBasicCommissioningInfoWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeRegulatoryConfigWithCompletionHandler:@
readAttributeRegulatoryConfigWithCompletionHandlerSelector :: Selector
readAttributeRegulatoryConfigWithCompletionHandlerSelector = mkSelector "readAttributeRegulatoryConfigWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeRegulatoryConfigWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRegulatoryConfigWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRegulatoryConfigWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRegulatoryConfigWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRegulatoryConfigWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRegulatoryConfigWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeRegulatoryConfigWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeRegulatoryConfigWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLocationCapabilityWithCompletionHandler:@
readAttributeLocationCapabilityWithCompletionHandlerSelector :: Selector
readAttributeLocationCapabilityWithCompletionHandlerSelector = mkSelector "readAttributeLocationCapabilityWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeLocationCapabilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationCapabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLocationCapabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocationCapabilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocationCapabilityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLocationCapabilityWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLocationCapabilityWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLocationCapabilityWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSupportsConcurrentConnectionWithCompletionHandler:@
readAttributeSupportsConcurrentConnectionWithCompletionHandlerSelector :: Selector
readAttributeSupportsConcurrentConnectionWithCompletionHandlerSelector = mkSelector "readAttributeSupportsConcurrentConnectionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSupportsConcurrentConnectionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsConcurrentConnectionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportsConcurrentConnectionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportsConcurrentConnectionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportsConcurrentConnectionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportsConcurrentConnectionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSupportsConcurrentConnectionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSupportsConcurrentConnectionWithAttributeCache:endpoint:queue:completionHandler:"

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

