{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Bridged Device Basic Information
--
-- This Cluster serves two purposes towards a Node communicating with a Bridge: indicate that the functionality on          the Endpoint where it is placed (and its Parts) is bridged from a non-CHIP technology; and provide a centralized          collection of attributes that the Node MAY collect to aid in conveying information regarding the Bridged Device to a user,          such as the vendor name, the model name, or user-assigned name.
--
-- Generated bindings for @MTRBaseClusterBridgedDeviceBasicInformation@.
module ObjC.Matter.MTRBaseClusterBridgedDeviceBasicInformation
  ( MTRBaseClusterBridgedDeviceBasicInformation
  , IsMTRBaseClusterBridgedDeviceBasicInformation(..)
  , keepActiveWithParams_completion
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
  , readAttributeReachableWithCompletion
  , subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler
  , readAttributeReachableWithClusterStateCache_endpoint_queue_completion
  , readAttributeUniqueIDWithCompletion
  , subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductAppearanceWithCompletion
  , subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion
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
  , keepActiveWithParams_completionSelector
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
  , readAttributeReachableWithCompletionSelector
  , subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeReachableWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUniqueIDWithCompletionSelector
  , subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductAppearanceWithCompletionSelector
  , subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command KeepActive
--
-- Upon receipt, the server SHALL attempt to keep the bridged device active for the duration specified by the command, when the device is next active.
--
-- ObjC selector: @- keepActiveWithParams:completion:@
keepActiveWithParams_completion :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> IO ()
keepActiveWithParams_completion mtrBaseClusterBridgedDeviceBasicInformation  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "keepActiveWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeVendorNameWithCompletion:@
readAttributeVendorNameWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeVendorNameWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeVendorNameWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeVendorIDWithCompletion:@
readAttributeVendorIDWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeVendorIDWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeVendorIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProductNameWithCompletion:@
readAttributeProductNameWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeProductNameWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeProductNameWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProductNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeProductNameWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductNameWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProductIDWithCompletion:@
readAttributeProductIDWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeProductIDWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeProductIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNodeLabelWithCompletion:@
readAttributeNodeLabelWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeNodeLabelWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeNodeLabelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeNodeLabelWithValue:completion:@
writeAttributeNodeLabelWithValue_completion :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsNSString value) => mtrBaseClusterBridgedDeviceBasicInformation -> value -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_completion mtrBaseClusterBridgedDeviceBasicInformation  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "writeAttributeNodeLabelWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeNodeLabelWithValue:params:completion:@
writeAttributeNodeLabelWithValue_params_completion :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> value -> params -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_params_completion mtrBaseClusterBridgedDeviceBasicInformation  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "writeAttributeNodeLabelWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNodeLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeNodeLabelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNodeLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNodeLabelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeHardwareVersionWithCompletion:@
readAttributeHardwareVersionWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeHardwareVersionWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeHardwareVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeHardwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeHardwareVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHardwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHardwareVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeHardwareVersionStringWithCompletion:@
readAttributeHardwareVersionStringWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeHardwareVersionStringWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeHardwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeHardwareVersionStringWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHardwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHardwareVersionStringWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSoftwareVersionWithCompletion:@
readAttributeSoftwareVersionWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeSoftwareVersionWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeSoftwareVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSoftwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeSoftwareVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSoftwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSoftwareVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSoftwareVersionStringWithCompletion:@
readAttributeSoftwareVersionStringWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeSoftwareVersionStringWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSoftwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeSoftwareVersionStringWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSoftwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSoftwareVersionStringWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeManufacturingDateWithCompletion:@
readAttributeManufacturingDateWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeManufacturingDateWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeManufacturingDateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeManufacturingDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeManufacturingDateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeManufacturingDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeManufacturingDateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePartNumberWithCompletion:@
readAttributePartNumberWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributePartNumberWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributePartNumberWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePartNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributePartNumberWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePartNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributePartNumberWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePartNumberWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePartNumberWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProductURLWithCompletion:@
readAttributeProductURLWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeProductURLWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeProductURLWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProductURLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeProductURLWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductURLWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductURLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductURLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductURLWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProductLabelWithCompletion:@
readAttributeProductLabelWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeProductLabelWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeProductLabelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProductLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeProductLabelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductLabelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSerialNumberWithCompletion:@
readAttributeSerialNumberWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeSerialNumberWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeSerialNumberWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSerialNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeSerialNumberWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSerialNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSerialNumberWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeReachableWithCompletion:@
readAttributeReachableWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeReachableWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeReachableWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeReachableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeReachableWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeReachableWithClusterStateCache:endpoint:queue:completion:@
readAttributeReachableWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReachableWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeReachableWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeUniqueIDWithCompletion:@
readAttributeUniqueIDWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeUniqueIDWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeUniqueIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeUniqueIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeUniqueIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUniqueIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUniqueIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProductAppearanceWithCompletion:@
readAttributeProductAppearanceWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeProductAppearanceWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeProductAppearanceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeConfigurationVersionWithCompletion:@
readAttributeConfigurationVersionWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeConfigurationVersionWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeConfigurationVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeConfigurationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeConfigurationVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeConfigurationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeConfigurationVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterBridgedDeviceBasicInformation  completion =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> IO (Id MTRBaseClusterBridgedDeviceBasicInformation)
init_ mtrBaseClusterBridgedDeviceBasicInformation  =
    sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterBridgedDeviceBasicInformation)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterBridgedDeviceBasicInformation -> device -> endpointID -> queue -> IO (Id MTRBaseClusterBridgedDeviceBasicInformation)
initWithDevice_endpointID_queue mtrBaseClusterBridgedDeviceBasicInformation  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterBridgedDeviceBasicInformation (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keepActiveWithParams:completion:@
keepActiveWithParams_completionSelector :: Selector
keepActiveWithParams_completionSelector = mkSelector "keepActiveWithParams:completion:"

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

-- | @Selector@ for @readAttributeProductAppearanceWithCompletion:@
readAttributeProductAppearanceWithCompletionSelector :: Selector
readAttributeProductAppearanceWithCompletionSelector = mkSelector "readAttributeProductAppearanceWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:"

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

