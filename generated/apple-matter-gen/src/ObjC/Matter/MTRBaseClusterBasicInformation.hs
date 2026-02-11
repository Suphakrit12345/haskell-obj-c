{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Basic Information
--
-- This cluster provides attributes and events for determining basic information about Nodes, which supports both      Commissioning and operational determination of Node characteristics, such as Vendor ID, Product ID and serial number,      which apply to the whole Node. Also allows setting user device information such as location.
--
-- Generated bindings for @MTRBaseClusterBasicInformation@.
module ObjC.Matter.MTRBaseClusterBasicInformation
  ( MTRBaseClusterBasicInformation
  , IsMTRBaseClusterBasicInformation(..)
  , readAttributeDataModelRevisionWithCompletion
  , subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandler
  , readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completion
  , readAttributeVendorNameWithCompletion
  , subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeVendorIDWithCompletion
  , subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductNameWithCompletion
  , subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductIDWithCompletion
  , subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeNodeLabelWithCompletion
  , writeAttributeNodeLabelWithValue_completion
  , writeAttributeNodeLabelWithValue_params_completion
  , subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandler
  , readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completion
  , readAttributeLocationWithCompletion
  , writeAttributeLocationWithValue_completion
  , writeAttributeLocationWithValue_params_completion
  , subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandler
  , readAttributeLocationWithClusterStateCache_endpoint_queue_completion
  , readAttributeHardwareVersionWithCompletion
  , subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeHardwareVersionStringWithCompletion
  , subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandler
  , readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completion
  , readAttributeSoftwareVersionWithCompletion
  , subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeSoftwareVersionStringWithCompletion
  , subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandler
  , readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completion
  , readAttributeManufacturingDateWithCompletion
  , subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandler
  , readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completion
  , readAttributePartNumberWithCompletion
  , subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandler
  , readAttributePartNumberWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductURLWithCompletion
  , subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductURLWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductLabelWithCompletion
  , subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductLabelWithClusterStateCache_endpoint_queue_completion
  , readAttributeSerialNumberWithCompletion
  , subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandler
  , readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completion
  , readAttributeLocalConfigDisabledWithCompletion
  , writeAttributeLocalConfigDisabledWithValue_completion
  , writeAttributeLocalConfigDisabledWithValue_params_completion
  , subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completion
  , readAttributeReachableWithCompletion
  , subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler
  , readAttributeReachableWithClusterStateCache_endpoint_queue_completion
  , readAttributeUniqueIDWithCompletion
  , subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeCapabilityMinimaWithCompletion
  , subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandler
  , readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductAppearanceWithCompletion
  , subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion
  , readAttributeSpecificationVersionWithCompletion
  , subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxPathsPerInvokeWithCompletion
  , subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completion
  , readAttributeConfigurationVersionWithCompletion
  , subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeDataModelRevisionWithCompletionSelector
  , subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVendorNameWithCompletionSelector
  , subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVendorIDWithCompletionSelector
  , subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductNameWithCompletionSelector
  , subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductIDWithCompletionSelector
  , subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNodeLabelWithCompletionSelector
  , writeAttributeNodeLabelWithValue_completionSelector
  , writeAttributeNodeLabelWithValue_params_completionSelector
  , subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLocationWithCompletionSelector
  , writeAttributeLocationWithValue_completionSelector
  , writeAttributeLocationWithValue_params_completionSelector
  , subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLocationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHardwareVersionWithCompletionSelector
  , subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHardwareVersionStringWithCompletionSelector
  , subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSoftwareVersionWithCompletionSelector
  , subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSoftwareVersionStringWithCompletionSelector
  , subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeManufacturingDateWithCompletionSelector
  , subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePartNumberWithCompletionSelector
  , subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePartNumberWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductURLWithCompletionSelector
  , subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductURLWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductLabelWithCompletionSelector
  , subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductLabelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSerialNumberWithCompletionSelector
  , subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLocalConfigDisabledWithCompletionSelector
  , writeAttributeLocalConfigDisabledWithValue_completionSelector
  , writeAttributeLocalConfigDisabledWithValue_params_completionSelector
  , subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeReachableWithCompletionSelector
  , subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeReachableWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUniqueIDWithCompletionSelector
  , subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCapabilityMinimaWithCompletionSelector
  , subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductAppearanceWithCompletionSelector
  , subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSpecificationVersionWithCompletionSelector
  , subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxPathsPerInvokeWithCompletionSelector
  , subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeConfigurationVersionWithCompletionSelector
  , subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributeDataModelRevisionWithCompletion:@
readAttributeDataModelRevisionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeDataModelRevisionWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeDataModelRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDataModelRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeDataModelRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDataModelRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDataModelRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeVendorNameWithCompletion:@
readAttributeVendorNameWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeVendorNameWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeVendorNameWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeVendorIDWithCompletion:@
readAttributeVendorIDWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeVendorIDWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeVendorIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProductNameWithCompletion:@
readAttributeProductNameWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeProductNameWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeProductNameWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProductNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeProductNameWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductNameWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProductIDWithCompletion:@
readAttributeProductIDWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeProductIDWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeProductIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNodeLabelWithCompletion:@
readAttributeNodeLabelWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeNodeLabelWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeNodeLabelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeNodeLabelWithValue:completion:@
writeAttributeNodeLabelWithValue_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSString value) => mtrBaseClusterBasicInformation -> value -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_completion mtrBaseClusterBasicInformation  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "writeAttributeNodeLabelWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeNodeLabelWithValue:params:completion:@
writeAttributeNodeLabelWithValue_params_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBasicInformation -> value -> params -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_params_completion mtrBaseClusterBasicInformation  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBasicInformation (mkSelector "writeAttributeNodeLabelWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNodeLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeNodeLabelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNodeLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNodeLabelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLocationWithCompletion:@
readAttributeLocationWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeLocationWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeLocationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLocationWithValue:completion:@
writeAttributeLocationWithValue_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSString value) => mtrBaseClusterBasicInformation -> value -> Ptr () -> IO ()
writeAttributeLocationWithValue_completion mtrBaseClusterBasicInformation  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "writeAttributeLocationWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLocationWithValue:params:completion:@
writeAttributeLocationWithValue_params_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBasicInformation -> value -> params -> Ptr () -> IO ()
writeAttributeLocationWithValue_params_completion mtrBaseClusterBasicInformation  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBasicInformation (mkSelector "writeAttributeLocationWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLocationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeLocationWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLocationWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLocationWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeHardwareVersionWithCompletion:@
readAttributeHardwareVersionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeHardwareVersionWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeHardwareVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeHardwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeHardwareVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHardwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHardwareVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeHardwareVersionStringWithCompletion:@
readAttributeHardwareVersionStringWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeHardwareVersionStringWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeHardwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeHardwareVersionStringWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHardwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHardwareVersionStringWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSoftwareVersionWithCompletion:@
readAttributeSoftwareVersionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeSoftwareVersionWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeSoftwareVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSoftwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeSoftwareVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSoftwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSoftwareVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSoftwareVersionStringWithCompletion:@
readAttributeSoftwareVersionStringWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeSoftwareVersionStringWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSoftwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeSoftwareVersionStringWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSoftwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSoftwareVersionStringWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeManufacturingDateWithCompletion:@
readAttributeManufacturingDateWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeManufacturingDateWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeManufacturingDateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeManufacturingDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeManufacturingDateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeManufacturingDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeManufacturingDateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePartNumberWithCompletion:@
readAttributePartNumberWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributePartNumberWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributePartNumberWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePartNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributePartNumberWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePartNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributePartNumberWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePartNumberWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePartNumberWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProductURLWithCompletion:@
readAttributeProductURLWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeProductURLWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeProductURLWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProductURLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeProductURLWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductURLWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductURLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductURLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductURLWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProductLabelWithCompletion:@
readAttributeProductLabelWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeProductLabelWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeProductLabelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProductLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeProductLabelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductLabelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSerialNumberWithCompletion:@
readAttributeSerialNumberWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeSerialNumberWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeSerialNumberWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSerialNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeSerialNumberWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSerialNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSerialNumberWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLocalConfigDisabledWithCompletion:@
readAttributeLocalConfigDisabledWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeLocalConfigDisabledWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeLocalConfigDisabledWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLocalConfigDisabledWithValue:completion:@
writeAttributeLocalConfigDisabledWithValue_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSNumber value) => mtrBaseClusterBasicInformation -> value -> Ptr () -> IO ()
writeAttributeLocalConfigDisabledWithValue_completion mtrBaseClusterBasicInformation  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "writeAttributeLocalConfigDisabledWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLocalConfigDisabledWithValue:params:completion:@
writeAttributeLocalConfigDisabledWithValue_params_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBasicInformation -> value -> params -> Ptr () -> IO ()
writeAttributeLocalConfigDisabledWithValue_params_completion mtrBaseClusterBasicInformation  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBasicInformation (mkSelector "writeAttributeLocalConfigDisabledWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLocalConfigDisabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeLocalConfigDisabledWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLocalConfigDisabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLocalConfigDisabledWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeReachableWithCompletion:@
readAttributeReachableWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeReachableWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeReachableWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeReachableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeReachableWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeReachableWithClusterStateCache:endpoint:queue:completion:@
readAttributeReachableWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReachableWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeReachableWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeUniqueIDWithCompletion:@
readAttributeUniqueIDWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeUniqueIDWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeUniqueIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeUniqueIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeUniqueIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUniqueIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUniqueIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCapabilityMinimaWithCompletion:@
readAttributeCapabilityMinimaWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeCapabilityMinimaWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeCapabilityMinimaWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCapabilityMinimaWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeCapabilityMinimaWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCapabilityMinimaWithClusterStateCache:endpoint:queue:completion:@
readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCapabilityMinimaWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProductAppearanceWithCompletion:@
readAttributeProductAppearanceWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeProductAppearanceWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeProductAppearanceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSpecificationVersionWithCompletion:@
readAttributeSpecificationVersionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeSpecificationVersionWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeSpecificationVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSpecificationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeSpecificationVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSpecificationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSpecificationVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxPathsPerInvokeWithCompletion:@
readAttributeMaxPathsPerInvokeWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeMaxPathsPerInvokeWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeMaxPathsPerInvokeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxPathsPerInvokeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeMaxPathsPerInvokeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxPathsPerInvokeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxPathsPerInvokeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeConfigurationVersionWithCompletion:@
readAttributeConfigurationVersionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeConfigurationVersionWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeConfigurationVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeConfigurationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeConfigurationVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeConfigurationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeConfigurationVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterBasicInformation  completion =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasicInformation (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> IO (Id MTRBaseClusterBasicInformation)
init_ mtrBaseClusterBasicInformation  =
    sendMsg mtrBaseClusterBasicInformation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterBasicInformation)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterBasicInformation -> device -> endpointID -> queue -> IO (Id MTRBaseClusterBasicInformation)
initWithDevice_endpointID_queue mtrBaseClusterBasicInformation  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterBasicInformation (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeDataModelRevisionWithCompletion:@
readAttributeDataModelRevisionWithCompletionSelector :: Selector
readAttributeDataModelRevisionWithCompletionSelector = mkSelector "readAttributeDataModelRevisionWithCompletion:"

-- | @Selector@ for @subscribeAttributeDataModelRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDataModelRevisionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDataModelRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDataModelRevisionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeVendorNameWithCompletion:@
readAttributeVendorNameWithCompletionSelector :: Selector
readAttributeVendorNameWithCompletionSelector = mkSelector "readAttributeVendorNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeVendorIDWithCompletion:@
readAttributeVendorIDWithCompletionSelector :: Selector
readAttributeVendorIDWithCompletionSelector = mkSelector "readAttributeVendorIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductNameWithCompletion:@
readAttributeProductNameWithCompletionSelector :: Selector
readAttributeProductNameWithCompletionSelector = mkSelector "readAttributeProductNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProductNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductIDWithCompletion:@
readAttributeProductIDWithCompletionSelector :: Selector
readAttributeProductIDWithCompletionSelector = mkSelector "readAttributeProductIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNodeLabelWithCompletion:@
readAttributeNodeLabelWithCompletionSelector :: Selector
readAttributeNodeLabelWithCompletionSelector = mkSelector "readAttributeNodeLabelWithCompletion:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:completion:@
writeAttributeNodeLabelWithValue_completionSelector :: Selector
writeAttributeNodeLabelWithValue_completionSelector = mkSelector "writeAttributeNodeLabelWithValue:completion:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:params:completion:@
writeAttributeNodeLabelWithValue_params_completionSelector :: Selector
writeAttributeNodeLabelWithValue_params_completionSelector = mkSelector "writeAttributeNodeLabelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeNodeLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeLabelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNodeLabelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLocationWithCompletion:@
readAttributeLocationWithCompletionSelector :: Selector
readAttributeLocationWithCompletionSelector = mkSelector "readAttributeLocationWithCompletion:"

-- | @Selector@ for @writeAttributeLocationWithValue:completion:@
writeAttributeLocationWithValue_completionSelector :: Selector
writeAttributeLocationWithValue_completionSelector = mkSelector "writeAttributeLocationWithValue:completion:"

-- | @Selector@ for @writeAttributeLocationWithValue:params:completion:@
writeAttributeLocationWithValue_params_completionSelector :: Selector
writeAttributeLocationWithValue_params_completionSelector = mkSelector "writeAttributeLocationWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLocationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocationWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocationWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLocationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLocationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHardwareVersionWithCompletion:@
readAttributeHardwareVersionWithCompletionSelector :: Selector
readAttributeHardwareVersionWithCompletionSelector = mkSelector "readAttributeHardwareVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeHardwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHardwareVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHardwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHardwareVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHardwareVersionStringWithCompletion:@
readAttributeHardwareVersionStringWithCompletionSelector :: Selector
readAttributeHardwareVersionStringWithCompletionSelector = mkSelector "readAttributeHardwareVersionStringWithCompletion:"

-- | @Selector@ for @subscribeAttributeHardwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHardwareVersionStringWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHardwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHardwareVersionStringWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSoftwareVersionWithCompletion:@
readAttributeSoftwareVersionWithCompletionSelector :: Selector
readAttributeSoftwareVersionWithCompletionSelector = mkSelector "readAttributeSoftwareVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeSoftwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSoftwareVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSoftwareVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSoftwareVersionStringWithCompletion:@
readAttributeSoftwareVersionStringWithCompletionSelector :: Selector
readAttributeSoftwareVersionStringWithCompletionSelector = mkSelector "readAttributeSoftwareVersionStringWithCompletion:"

-- | @Selector@ for @subscribeAttributeSoftwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSoftwareVersionStringWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSoftwareVersionStringWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeManufacturingDateWithCompletion:@
readAttributeManufacturingDateWithCompletionSelector :: Selector
readAttributeManufacturingDateWithCompletionSelector = mkSelector "readAttributeManufacturingDateWithCompletion:"

-- | @Selector@ for @subscribeAttributeManufacturingDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeManufacturingDateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeManufacturingDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeManufacturingDateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePartNumberWithCompletion:@
readAttributePartNumberWithCompletionSelector :: Selector
readAttributePartNumberWithCompletionSelector = mkSelector "readAttributePartNumberWithCompletion:"

-- | @Selector@ for @subscribeAttributePartNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePartNumberWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePartNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributePartNumberWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePartNumberWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePartNumberWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductURLWithCompletion:@
readAttributeProductURLWithCompletionSelector :: Selector
readAttributeProductURLWithCompletionSelector = mkSelector "readAttributeProductURLWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductURLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductURLWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductURLWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductURLWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProductURLWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductURLWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductLabelWithCompletion:@
readAttributeProductLabelWithCompletionSelector :: Selector
readAttributeProductLabelWithCompletionSelector = mkSelector "readAttributeProductLabelWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductLabelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductLabelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSerialNumberWithCompletion:@
readAttributeSerialNumberWithCompletionSelector :: Selector
readAttributeSerialNumberWithCompletionSelector = mkSelector "readAttributeSerialNumberWithCompletion:"

-- | @Selector@ for @subscribeAttributeSerialNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSerialNumberWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSerialNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSerialNumberWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLocalConfigDisabledWithCompletion:@
readAttributeLocalConfigDisabledWithCompletionSelector :: Selector
readAttributeLocalConfigDisabledWithCompletionSelector = mkSelector "readAttributeLocalConfigDisabledWithCompletion:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:completion:@
writeAttributeLocalConfigDisabledWithValue_completionSelector :: Selector
writeAttributeLocalConfigDisabledWithValue_completionSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:completion:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:params:completion:@
writeAttributeLocalConfigDisabledWithValue_params_completionSelector :: Selector
writeAttributeLocalConfigDisabledWithValue_params_completionSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLocalConfigDisabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocalConfigDisabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocalConfigDisabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLocalConfigDisabledWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeReachableWithCompletion:@
readAttributeReachableWithCompletionSelector :: Selector
readAttributeReachableWithCompletionSelector = mkSelector "readAttributeReachableWithCompletion:"

-- | @Selector@ for @subscribeAttributeReachableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReachableWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReachableWithClusterStateCache:endpoint:queue:completion:@
readAttributeReachableWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeReachableWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeReachableWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUniqueIDWithCompletion:@
readAttributeUniqueIDWithCompletionSelector :: Selector
readAttributeUniqueIDWithCompletionSelector = mkSelector "readAttributeUniqueIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeUniqueIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUniqueIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUniqueIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUniqueIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCapabilityMinimaWithCompletion:@
readAttributeCapabilityMinimaWithCompletionSelector :: Selector
readAttributeCapabilityMinimaWithCompletionSelector = mkSelector "readAttributeCapabilityMinimaWithCompletion:"

-- | @Selector@ for @subscribeAttributeCapabilityMinimaWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCapabilityMinimaWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCapabilityMinimaWithClusterStateCache:endpoint:queue:completion:@
readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCapabilityMinimaWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductAppearanceWithCompletion:@
readAttributeProductAppearanceWithCompletionSelector :: Selector
readAttributeProductAppearanceWithCompletionSelector = mkSelector "readAttributeProductAppearanceWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSpecificationVersionWithCompletion:@
readAttributeSpecificationVersionWithCompletionSelector :: Selector
readAttributeSpecificationVersionWithCompletionSelector = mkSelector "readAttributeSpecificationVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeSpecificationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpecificationVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpecificationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpecificationVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxPathsPerInvokeWithCompletion:@
readAttributeMaxPathsPerInvokeWithCompletionSelector :: Selector
readAttributeMaxPathsPerInvokeWithCompletionSelector = mkSelector "readAttributeMaxPathsPerInvokeWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxPathsPerInvokeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxPathsPerInvokeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxPathsPerInvokeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxPathsPerInvokeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeConfigurationVersionWithCompletion:@
readAttributeConfigurationVersionWithCompletionSelector :: Selector
readAttributeConfigurationVersionWithCompletionSelector = mkSelector "readAttributeConfigurationVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeConfigurationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeConfigurationVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeConfigurationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeConfigurationVersionWithClusterStateCache:endpoint:queue:completion:"

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

