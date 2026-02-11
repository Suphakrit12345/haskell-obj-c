{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Network Commissioning
--
-- Functionality to configure, enable, disable network credentials and access on a Matter device.
--
-- Generated bindings for @MTRBaseClusterNetworkCommissioning@.
module ObjC.Matter.MTRBaseClusterNetworkCommissioning
  ( MTRBaseClusterNetworkCommissioning
  , IsMTRBaseClusterNetworkCommissioning(..)
  , scanNetworksWithParams_completion
  , scanNetworksWithCompletion
  , addOrUpdateWiFiNetworkWithParams_completion
  , addOrUpdateThreadNetworkWithParams_completion
  , removeNetworkWithParams_completion
  , connectNetworkWithParams_completion
  , reorderNetworkWithParams_completion
  , queryIdentityWithParams_completion
  , readAttributeMaxNetworksWithCompletion
  , subscribeAttributeMaxNetworksWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxNetworksWithClusterStateCache_endpoint_queue_completion
  , readAttributeNetworksWithCompletion
  , subscribeAttributeNetworksWithParams_subscriptionEstablished_reportHandler
  , readAttributeNetworksWithClusterStateCache_endpoint_queue_completion
  , readAttributeScanMaxTimeSecondsWithCompletion
  , subscribeAttributeScanMaxTimeSecondsWithParams_subscriptionEstablished_reportHandler
  , readAttributeScanMaxTimeSecondsWithClusterStateCache_endpoint_queue_completion
  , readAttributeConnectMaxTimeSecondsWithCompletion
  , subscribeAttributeConnectMaxTimeSecondsWithParams_subscriptionEstablished_reportHandler
  , readAttributeConnectMaxTimeSecondsWithClusterStateCache_endpoint_queue_completion
  , readAttributeInterfaceEnabledWithCompletion
  , writeAttributeInterfaceEnabledWithValue_completion
  , writeAttributeInterfaceEnabledWithValue_params_completion
  , subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completion
  , readAttributeLastNetworkingStatusWithCompletion
  , subscribeAttributeLastNetworkingStatusWithParams_subscriptionEstablished_reportHandler
  , readAttributeLastNetworkingStatusWithClusterStateCache_endpoint_queue_completion
  , readAttributeLastNetworkIDWithCompletion
  , subscribeAttributeLastNetworkIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeLastNetworkIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeLastConnectErrorValueWithCompletion
  , subscribeAttributeLastConnectErrorValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeLastConnectErrorValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedWiFiBandsWithCompletion
  , subscribeAttributeSupportedWiFiBandsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedWiFiBandsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedThreadFeaturesWithCompletion
  , subscribeAttributeSupportedThreadFeaturesWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedThreadFeaturesWithClusterStateCache_endpoint_queue_completion
  , readAttributeThreadVersionWithCompletion
  , subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completion
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
  , scanNetworksWithParams_completionHandler
  , addOrUpdateWiFiNetworkWithParams_completionHandler
  , addOrUpdateThreadNetworkWithParams_completionHandler
  , removeNetworkWithParams_completionHandler
  , connectNetworkWithParams_completionHandler
  , reorderNetworkWithParams_completionHandler
  , readAttributeMaxNetworksWithCompletionHandler
  , subscribeAttributeMaxNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMaxNetworksWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeNetworksWithCompletionHandler
  , subscribeAttributeNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeNetworksWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeScanMaxTimeSecondsWithCompletionHandler
  , subscribeAttributeScanMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeScanMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeConnectMaxTimeSecondsWithCompletionHandler
  , subscribeAttributeConnectMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeConnectMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeInterfaceEnabledWithCompletionHandler
  , writeAttributeInterfaceEnabledWithValue_completionHandler
  , writeAttributeInterfaceEnabledWithValue_params_completionHandler
  , subscribeAttributeInterfaceEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeInterfaceEnabledWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLastNetworkingStatusWithCompletionHandler
  , subscribeAttributeLastNetworkingStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLastNetworkingStatusWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLastNetworkIDWithCompletionHandler
  , subscribeAttributeLastNetworkIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLastNetworkIDWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLastConnectErrorValueWithCompletionHandler
  , subscribeAttributeLastConnectErrorValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLastConnectErrorValueWithAttributeCache_endpoint_queue_completionHandler
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
  , scanNetworksWithParams_completionSelector
  , scanNetworksWithCompletionSelector
  , addOrUpdateWiFiNetworkWithParams_completionSelector
  , addOrUpdateThreadNetworkWithParams_completionSelector
  , removeNetworkWithParams_completionSelector
  , connectNetworkWithParams_completionSelector
  , reorderNetworkWithParams_completionSelector
  , queryIdentityWithParams_completionSelector
  , readAttributeMaxNetworksWithCompletionSelector
  , subscribeAttributeMaxNetworksWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxNetworksWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNetworksWithCompletionSelector
  , subscribeAttributeNetworksWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNetworksWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeScanMaxTimeSecondsWithCompletionSelector
  , subscribeAttributeScanMaxTimeSecondsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeScanMaxTimeSecondsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeConnectMaxTimeSecondsWithCompletionSelector
  , subscribeAttributeConnectMaxTimeSecondsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeConnectMaxTimeSecondsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInterfaceEnabledWithCompletionSelector
  , writeAttributeInterfaceEnabledWithValue_completionSelector
  , writeAttributeInterfaceEnabledWithValue_params_completionSelector
  , subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLastNetworkingStatusWithCompletionSelector
  , subscribeAttributeLastNetworkingStatusWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLastNetworkingStatusWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLastNetworkIDWithCompletionSelector
  , subscribeAttributeLastNetworkIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLastNetworkIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLastConnectErrorValueWithCompletionSelector
  , subscribeAttributeLastConnectErrorValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLastConnectErrorValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedWiFiBandsWithCompletionSelector
  , subscribeAttributeSupportedWiFiBandsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedWiFiBandsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedThreadFeaturesWithCompletionSelector
  , subscribeAttributeSupportedThreadFeaturesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedThreadFeaturesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeThreadVersionWithCompletionSelector
  , subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completionSelector
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
  , scanNetworksWithParams_completionHandlerSelector
  , addOrUpdateWiFiNetworkWithParams_completionHandlerSelector
  , addOrUpdateThreadNetworkWithParams_completionHandlerSelector
  , removeNetworkWithParams_completionHandlerSelector
  , connectNetworkWithParams_completionHandlerSelector
  , reorderNetworkWithParams_completionHandlerSelector
  , readAttributeMaxNetworksWithCompletionHandlerSelector
  , subscribeAttributeMaxNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxNetworksWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeNetworksWithCompletionHandlerSelector
  , subscribeAttributeNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeNetworksWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeScanMaxTimeSecondsWithCompletionHandlerSelector
  , subscribeAttributeScanMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeScanMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeConnectMaxTimeSecondsWithCompletionHandlerSelector
  , subscribeAttributeConnectMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeConnectMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeInterfaceEnabledWithCompletionHandlerSelector
  , writeAttributeInterfaceEnabledWithValue_completionHandlerSelector
  , writeAttributeInterfaceEnabledWithValue_params_completionHandlerSelector
  , subscribeAttributeInterfaceEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeInterfaceEnabledWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLastNetworkingStatusWithCompletionHandlerSelector
  , subscribeAttributeLastNetworkingStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLastNetworkingStatusWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLastNetworkIDWithCompletionHandlerSelector
  , subscribeAttributeLastNetworkIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLastNetworkIDWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLastConnectErrorValueWithCompletionHandlerSelector
  , subscribeAttributeLastConnectErrorValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLastConnectErrorValueWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command ScanNetworks
--
-- Detemine the set of networks the device sees as available.
--
-- ObjC selector: @- scanNetworksWithParams:completion:@
scanNetworksWithParams_completion :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterScanNetworksParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
scanNetworksWithParams_completion mtrBaseClusterNetworkCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "scanNetworksWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- scanNetworksWithCompletion:@
scanNetworksWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
scanNetworksWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "scanNetworksWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command AddOrUpdateWiFiNetwork
--
-- Add or update the credentials for a given Wi-Fi network.
--
-- ObjC selector: @- addOrUpdateWiFiNetworkWithParams:completion:@
addOrUpdateWiFiNetworkWithParams_completion :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
addOrUpdateWiFiNetworkWithParams_completion mtrBaseClusterNetworkCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "addOrUpdateWiFiNetworkWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command AddOrUpdateThreadNetwork
--
-- Add or update the credentials for a given Thread network.
--
-- ObjC selector: @- addOrUpdateThreadNetworkWithParams:completion:@
addOrUpdateThreadNetworkWithParams_completion :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
addOrUpdateThreadNetworkWithParams_completion mtrBaseClusterNetworkCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "addOrUpdateThreadNetworkWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveNetwork
--
-- Remove the definition of a given network (including its credentials).
--
-- ObjC selector: @- removeNetworkWithParams:completion:@
removeNetworkWithParams_completion :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterRemoveNetworkParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
removeNetworkWithParams_completion mtrBaseClusterNetworkCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "removeNetworkWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ConnectNetwork
--
-- Connect to the specified network, using previously-defined credentials.
--
-- ObjC selector: @- connectNetworkWithParams:completion:@
connectNetworkWithParams_completion :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterConnectNetworkParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
connectNetworkWithParams_completion mtrBaseClusterNetworkCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "connectNetworkWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ReorderNetwork
--
-- Modify the order in which networks will be presented in the Networks attribute.
--
-- ObjC selector: @- reorderNetworkWithParams:completion:@
reorderNetworkWithParams_completion :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterReorderNetworkParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
reorderNetworkWithParams_completion mtrBaseClusterNetworkCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "reorderNetworkWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command QueryIdentity
--
-- Retrieve details about and optionally proof of possession of a network client identity.
--
-- ObjC selector: @- queryIdentityWithParams:completion:@
queryIdentityWithParams_completion :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterQueryIdentityParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
queryIdentityWithParams_completion mtrBaseClusterNetworkCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "queryIdentityWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxNetworksWithCompletion:@
readAttributeMaxNetworksWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeMaxNetworksWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeMaxNetworksWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxNetworksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxNetworksWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxNetworksWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeMaxNetworksWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxNetworksWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxNetworksWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxNetworksWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxNetworksWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNetworksWithCompletion:@
readAttributeNetworksWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeNetworksWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeNetworksWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNetworksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNetworksWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNetworksWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeNetworksWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNetworksWithClusterStateCache:endpoint:queue:completion:@
readAttributeNetworksWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNetworksWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNetworksWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeScanMaxTimeSecondsWithCompletion:@
readAttributeScanMaxTimeSecondsWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeScanMaxTimeSecondsWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeScanMaxTimeSecondsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeScanMaxTimeSecondsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScanMaxTimeSecondsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScanMaxTimeSecondsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeScanMaxTimeSecondsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScanMaxTimeSecondsWithClusterStateCache:endpoint:queue:completion:@
readAttributeScanMaxTimeSecondsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScanMaxTimeSecondsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScanMaxTimeSecondsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeConnectMaxTimeSecondsWithCompletion:@
readAttributeConnectMaxTimeSecondsWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeConnectMaxTimeSecondsWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeConnectMaxTimeSecondsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeConnectMaxTimeSecondsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConnectMaxTimeSecondsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeConnectMaxTimeSecondsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeConnectMaxTimeSecondsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeConnectMaxTimeSecondsWithClusterStateCache:endpoint:queue:completion:@
readAttributeConnectMaxTimeSecondsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeConnectMaxTimeSecondsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeConnectMaxTimeSecondsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeInterfaceEnabledWithCompletion:@
readAttributeInterfaceEnabledWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeInterfaceEnabledWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeInterfaceEnabledWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeInterfaceEnabledWithValue:completion:@
writeAttributeInterfaceEnabledWithValue_completion :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber value) => mtrBaseClusterNetworkCommissioning -> value -> Ptr () -> IO ()
writeAttributeInterfaceEnabledWithValue_completion mtrBaseClusterNetworkCommissioning  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "writeAttributeInterfaceEnabledWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeInterfaceEnabledWithValue:params:completion:@
writeAttributeInterfaceEnabledWithValue_params_completion :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterNetworkCommissioning -> value -> params -> Ptr () -> IO ()
writeAttributeInterfaceEnabledWithValue_params_completion mtrBaseClusterNetworkCommissioning  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "writeAttributeInterfaceEnabledWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeInterfaceEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeInterfaceEnabledWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeInterfaceEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeInterfaceEnabledWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLastNetworkingStatusWithCompletion:@
readAttributeLastNetworkingStatusWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeLastNetworkingStatusWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeLastNetworkingStatusWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLastNetworkingStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLastNetworkingStatusWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLastNetworkingStatusWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeLastNetworkingStatusWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLastNetworkingStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeLastNetworkingStatusWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLastNetworkingStatusWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLastNetworkingStatusWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLastNetworkIDWithCompletion:@
readAttributeLastNetworkIDWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeLastNetworkIDWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeLastNetworkIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLastNetworkIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLastNetworkIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLastNetworkIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeLastNetworkIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLastNetworkIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeLastNetworkIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLastNetworkIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLastNetworkIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLastConnectErrorValueWithCompletion:@
readAttributeLastConnectErrorValueWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeLastConnectErrorValueWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeLastConnectErrorValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLastConnectErrorValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLastConnectErrorValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLastConnectErrorValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeLastConnectErrorValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLastConnectErrorValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeLastConnectErrorValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLastConnectErrorValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLastConnectErrorValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedWiFiBandsWithCompletion:@
readAttributeSupportedWiFiBandsWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeSupportedWiFiBandsWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeSupportedWiFiBandsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedWiFiBandsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedWiFiBandsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedWiFiBandsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeSupportedWiFiBandsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedWiFiBandsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedWiFiBandsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedWiFiBandsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedWiFiBandsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedThreadFeaturesWithCompletion:@
readAttributeSupportedThreadFeaturesWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeSupportedThreadFeaturesWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeSupportedThreadFeaturesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedThreadFeaturesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedThreadFeaturesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedThreadFeaturesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeSupportedThreadFeaturesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedThreadFeaturesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedThreadFeaturesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedThreadFeaturesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedThreadFeaturesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeThreadVersionWithCompletion:@
readAttributeThreadVersionWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeThreadVersionWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeThreadVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeThreadVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeThreadVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeThreadVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeThreadVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterNetworkCommissioning  completion =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> IO (Id MTRBaseClusterNetworkCommissioning)
init_ mtrBaseClusterNetworkCommissioning  =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterNetworkCommissioning)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterNetworkCommissioning -> device -> CUShort -> queue -> IO (Id MTRBaseClusterNetworkCommissioning)
initWithDevice_endpoint_queue mtrBaseClusterNetworkCommissioning  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- scanNetworksWithParams:completionHandler:@
scanNetworksWithParams_completionHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterScanNetworksParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
scanNetworksWithParams_completionHandler mtrBaseClusterNetworkCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "scanNetworksWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addOrUpdateWiFiNetworkWithParams:completionHandler:@
addOrUpdateWiFiNetworkWithParams_completionHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterAddOrUpdateWiFiNetworkParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
addOrUpdateWiFiNetworkWithParams_completionHandler mtrBaseClusterNetworkCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "addOrUpdateWiFiNetworkWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addOrUpdateThreadNetworkWithParams:completionHandler:@
addOrUpdateThreadNetworkWithParams_completionHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
addOrUpdateThreadNetworkWithParams_completionHandler mtrBaseClusterNetworkCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "addOrUpdateThreadNetworkWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removeNetworkWithParams:completionHandler:@
removeNetworkWithParams_completionHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterRemoveNetworkParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
removeNetworkWithParams_completionHandler mtrBaseClusterNetworkCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "removeNetworkWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- connectNetworkWithParams:completionHandler:@
connectNetworkWithParams_completionHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterConnectNetworkParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
connectNetworkWithParams_completionHandler mtrBaseClusterNetworkCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "connectNetworkWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- reorderNetworkWithParams:completionHandler:@
reorderNetworkWithParams_completionHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRNetworkCommissioningClusterReorderNetworkParams params) => mtrBaseClusterNetworkCommissioning -> params -> Ptr () -> IO ()
reorderNetworkWithParams_completionHandler mtrBaseClusterNetworkCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "reorderNetworkWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMaxNetworksWithCompletionHandler:@
readAttributeMaxNetworksWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeMaxNetworksWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeMaxNetworksWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMaxNetworksWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeMaxNetworksWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxNetworksWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxNetworksWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxNetworksWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxNetworksWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeNetworksWithCompletionHandler:@
readAttributeNetworksWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeNetworksWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeNetworksWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeNetworksWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeNetworksWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNetworksWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNetworksWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNetworksWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNetworksWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeScanMaxTimeSecondsWithCompletionHandler:@
readAttributeScanMaxTimeSecondsWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeScanMaxTimeSecondsWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeScanMaxTimeSecondsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeScanMaxTimeSecondsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeScanMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScanMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeScanMaxTimeSecondsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScanMaxTimeSecondsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeScanMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScanMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScanMaxTimeSecondsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeConnectMaxTimeSecondsWithCompletionHandler:@
readAttributeConnectMaxTimeSecondsWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeConnectMaxTimeSecondsWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeConnectMaxTimeSecondsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeConnectMaxTimeSecondsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeConnectMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeConnectMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeConnectMaxTimeSecondsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeConnectMaxTimeSecondsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeConnectMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeConnectMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeConnectMaxTimeSecondsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeInterfaceEnabledWithCompletionHandler:@
readAttributeInterfaceEnabledWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeInterfaceEnabledWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeInterfaceEnabledWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeInterfaceEnabledWithValue:completionHandler:@
writeAttributeInterfaceEnabledWithValue_completionHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber value) => mtrBaseClusterNetworkCommissioning -> value -> Ptr () -> IO ()
writeAttributeInterfaceEnabledWithValue_completionHandler mtrBaseClusterNetworkCommissioning  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "writeAttributeInterfaceEnabledWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeInterfaceEnabledWithValue:params:completionHandler:@
writeAttributeInterfaceEnabledWithValue_params_completionHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterNetworkCommissioning -> value -> params -> Ptr () -> IO ()
writeAttributeInterfaceEnabledWithValue_params_completionHandler mtrBaseClusterNetworkCommissioning  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "writeAttributeInterfaceEnabledWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeInterfaceEnabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeInterfaceEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInterfaceEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeInterfaceEnabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeInterfaceEnabledWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeInterfaceEnabledWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInterfaceEnabledWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeInterfaceEnabledWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLastNetworkingStatusWithCompletionHandler:@
readAttributeLastNetworkingStatusWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeLastNetworkingStatusWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeLastNetworkingStatusWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLastNetworkingStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLastNetworkingStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLastNetworkingStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeLastNetworkingStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLastNetworkingStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLastNetworkingStatusWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLastNetworkingStatusWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLastNetworkingStatusWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLastNetworkIDWithCompletionHandler:@
readAttributeLastNetworkIDWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeLastNetworkIDWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeLastNetworkIDWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLastNetworkIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLastNetworkIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLastNetworkIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeLastNetworkIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLastNetworkIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLastNetworkIDWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLastNetworkIDWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLastNetworkIDWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLastConnectErrorValueWithCompletionHandler:@
readAttributeLastConnectErrorValueWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeLastConnectErrorValueWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeLastConnectErrorValueWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLastConnectErrorValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLastConnectErrorValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLastConnectErrorValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeLastConnectErrorValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLastConnectErrorValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLastConnectErrorValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLastConnectErrorValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLastConnectErrorValueWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning => mtrBaseClusterNetworkCommissioning -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterNetworkCommissioning  completionHandler =
    sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterNetworkCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterNetworkCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterNetworkCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterNetworkCommissioning mtrBaseClusterNetworkCommissioning, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterNetworkCommissioning -> device -> endpointID -> queue -> IO (Id MTRBaseClusterNetworkCommissioning)
initWithDevice_endpointID_queue mtrBaseClusterNetworkCommissioning  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterNetworkCommissioning (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scanNetworksWithParams:completion:@
scanNetworksWithParams_completionSelector :: Selector
scanNetworksWithParams_completionSelector = mkSelector "scanNetworksWithParams:completion:"

-- | @Selector@ for @scanNetworksWithCompletion:@
scanNetworksWithCompletionSelector :: Selector
scanNetworksWithCompletionSelector = mkSelector "scanNetworksWithCompletion:"

-- | @Selector@ for @addOrUpdateWiFiNetworkWithParams:completion:@
addOrUpdateWiFiNetworkWithParams_completionSelector :: Selector
addOrUpdateWiFiNetworkWithParams_completionSelector = mkSelector "addOrUpdateWiFiNetworkWithParams:completion:"

-- | @Selector@ for @addOrUpdateThreadNetworkWithParams:completion:@
addOrUpdateThreadNetworkWithParams_completionSelector :: Selector
addOrUpdateThreadNetworkWithParams_completionSelector = mkSelector "addOrUpdateThreadNetworkWithParams:completion:"

-- | @Selector@ for @removeNetworkWithParams:completion:@
removeNetworkWithParams_completionSelector :: Selector
removeNetworkWithParams_completionSelector = mkSelector "removeNetworkWithParams:completion:"

-- | @Selector@ for @connectNetworkWithParams:completion:@
connectNetworkWithParams_completionSelector :: Selector
connectNetworkWithParams_completionSelector = mkSelector "connectNetworkWithParams:completion:"

-- | @Selector@ for @reorderNetworkWithParams:completion:@
reorderNetworkWithParams_completionSelector :: Selector
reorderNetworkWithParams_completionSelector = mkSelector "reorderNetworkWithParams:completion:"

-- | @Selector@ for @queryIdentityWithParams:completion:@
queryIdentityWithParams_completionSelector :: Selector
queryIdentityWithParams_completionSelector = mkSelector "queryIdentityWithParams:completion:"

-- | @Selector@ for @readAttributeMaxNetworksWithCompletion:@
readAttributeMaxNetworksWithCompletionSelector :: Selector
readAttributeMaxNetworksWithCompletionSelector = mkSelector "readAttributeMaxNetworksWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxNetworksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxNetworksWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxNetworksWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxNetworksWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxNetworksWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxNetworksWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxNetworksWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxNetworksWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNetworksWithCompletion:@
readAttributeNetworksWithCompletionSelector :: Selector
readAttributeNetworksWithCompletionSelector = mkSelector "readAttributeNetworksWithCompletion:"

-- | @Selector@ for @subscribeAttributeNetworksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNetworksWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNetworksWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNetworksWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNetworksWithClusterStateCache:endpoint:queue:completion:@
readAttributeNetworksWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNetworksWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNetworksWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeScanMaxTimeSecondsWithCompletion:@
readAttributeScanMaxTimeSecondsWithCompletionSelector :: Selector
readAttributeScanMaxTimeSecondsWithCompletionSelector = mkSelector "readAttributeScanMaxTimeSecondsWithCompletion:"

-- | @Selector@ for @subscribeAttributeScanMaxTimeSecondsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScanMaxTimeSecondsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScanMaxTimeSecondsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScanMaxTimeSecondsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScanMaxTimeSecondsWithClusterStateCache:endpoint:queue:completion:@
readAttributeScanMaxTimeSecondsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeScanMaxTimeSecondsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeScanMaxTimeSecondsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeConnectMaxTimeSecondsWithCompletion:@
readAttributeConnectMaxTimeSecondsWithCompletionSelector :: Selector
readAttributeConnectMaxTimeSecondsWithCompletionSelector = mkSelector "readAttributeConnectMaxTimeSecondsWithCompletion:"

-- | @Selector@ for @subscribeAttributeConnectMaxTimeSecondsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConnectMaxTimeSecondsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeConnectMaxTimeSecondsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeConnectMaxTimeSecondsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeConnectMaxTimeSecondsWithClusterStateCache:endpoint:queue:completion:@
readAttributeConnectMaxTimeSecondsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeConnectMaxTimeSecondsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeConnectMaxTimeSecondsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeInterfaceEnabledWithCompletion:@
readAttributeInterfaceEnabledWithCompletionSelector :: Selector
readAttributeInterfaceEnabledWithCompletionSelector = mkSelector "readAttributeInterfaceEnabledWithCompletion:"

-- | @Selector@ for @writeAttributeInterfaceEnabledWithValue:completion:@
writeAttributeInterfaceEnabledWithValue_completionSelector :: Selector
writeAttributeInterfaceEnabledWithValue_completionSelector = mkSelector "writeAttributeInterfaceEnabledWithValue:completion:"

-- | @Selector@ for @writeAttributeInterfaceEnabledWithValue:params:completion:@
writeAttributeInterfaceEnabledWithValue_params_completionSelector :: Selector
writeAttributeInterfaceEnabledWithValue_params_completionSelector = mkSelector "writeAttributeInterfaceEnabledWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeInterfaceEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInterfaceEnabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInterfaceEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInterfaceEnabledWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLastNetworkingStatusWithCompletion:@
readAttributeLastNetworkingStatusWithCompletionSelector :: Selector
readAttributeLastNetworkingStatusWithCompletionSelector = mkSelector "readAttributeLastNetworkingStatusWithCompletion:"

-- | @Selector@ for @subscribeAttributeLastNetworkingStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLastNetworkingStatusWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLastNetworkingStatusWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLastNetworkingStatusWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLastNetworkingStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeLastNetworkingStatusWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLastNetworkingStatusWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLastNetworkingStatusWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLastNetworkIDWithCompletion:@
readAttributeLastNetworkIDWithCompletionSelector :: Selector
readAttributeLastNetworkIDWithCompletionSelector = mkSelector "readAttributeLastNetworkIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeLastNetworkIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLastNetworkIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLastNetworkIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLastNetworkIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLastNetworkIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeLastNetworkIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLastNetworkIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLastNetworkIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLastConnectErrorValueWithCompletion:@
readAttributeLastConnectErrorValueWithCompletionSelector :: Selector
readAttributeLastConnectErrorValueWithCompletionSelector = mkSelector "readAttributeLastConnectErrorValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeLastConnectErrorValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLastConnectErrorValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLastConnectErrorValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLastConnectErrorValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLastConnectErrorValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeLastConnectErrorValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLastConnectErrorValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLastConnectErrorValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedWiFiBandsWithCompletion:@
readAttributeSupportedWiFiBandsWithCompletionSelector :: Selector
readAttributeSupportedWiFiBandsWithCompletionSelector = mkSelector "readAttributeSupportedWiFiBandsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedWiFiBandsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedWiFiBandsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedWiFiBandsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedWiFiBandsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedWiFiBandsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedWiFiBandsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedWiFiBandsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedWiFiBandsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedThreadFeaturesWithCompletion:@
readAttributeSupportedThreadFeaturesWithCompletionSelector :: Selector
readAttributeSupportedThreadFeaturesWithCompletionSelector = mkSelector "readAttributeSupportedThreadFeaturesWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedThreadFeaturesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedThreadFeaturesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedThreadFeaturesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedThreadFeaturesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedThreadFeaturesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedThreadFeaturesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedThreadFeaturesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedThreadFeaturesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeThreadVersionWithCompletion:@
readAttributeThreadVersionWithCompletionSelector :: Selector
readAttributeThreadVersionWithCompletionSelector = mkSelector "readAttributeThreadVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeThreadVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeThreadVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeThreadVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeThreadVersionWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @scanNetworksWithParams:completionHandler:@
scanNetworksWithParams_completionHandlerSelector :: Selector
scanNetworksWithParams_completionHandlerSelector = mkSelector "scanNetworksWithParams:completionHandler:"

-- | @Selector@ for @addOrUpdateWiFiNetworkWithParams:completionHandler:@
addOrUpdateWiFiNetworkWithParams_completionHandlerSelector :: Selector
addOrUpdateWiFiNetworkWithParams_completionHandlerSelector = mkSelector "addOrUpdateWiFiNetworkWithParams:completionHandler:"

-- | @Selector@ for @addOrUpdateThreadNetworkWithParams:completionHandler:@
addOrUpdateThreadNetworkWithParams_completionHandlerSelector :: Selector
addOrUpdateThreadNetworkWithParams_completionHandlerSelector = mkSelector "addOrUpdateThreadNetworkWithParams:completionHandler:"

-- | @Selector@ for @removeNetworkWithParams:completionHandler:@
removeNetworkWithParams_completionHandlerSelector :: Selector
removeNetworkWithParams_completionHandlerSelector = mkSelector "removeNetworkWithParams:completionHandler:"

-- | @Selector@ for @connectNetworkWithParams:completionHandler:@
connectNetworkWithParams_completionHandlerSelector :: Selector
connectNetworkWithParams_completionHandlerSelector = mkSelector "connectNetworkWithParams:completionHandler:"

-- | @Selector@ for @reorderNetworkWithParams:completionHandler:@
reorderNetworkWithParams_completionHandlerSelector :: Selector
reorderNetworkWithParams_completionHandlerSelector = mkSelector "reorderNetworkWithParams:completionHandler:"

-- | @Selector@ for @readAttributeMaxNetworksWithCompletionHandler:@
readAttributeMaxNetworksWithCompletionHandlerSelector :: Selector
readAttributeMaxNetworksWithCompletionHandlerSelector = mkSelector "readAttributeMaxNetworksWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMaxNetworksWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxNetworksWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxNetworksWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxNetworksWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMaxNetworksWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMaxNetworksWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeNetworksWithCompletionHandler:@
readAttributeNetworksWithCompletionHandlerSelector :: Selector
readAttributeNetworksWithCompletionHandlerSelector = mkSelector "readAttributeNetworksWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeNetworksWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNetworksWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNetworksWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNetworksWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNetworksWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeNetworksWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeNetworksWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeScanMaxTimeSecondsWithCompletionHandler:@
readAttributeScanMaxTimeSecondsWithCompletionHandlerSelector :: Selector
readAttributeScanMaxTimeSecondsWithCompletionHandlerSelector = mkSelector "readAttributeScanMaxTimeSecondsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeScanMaxTimeSecondsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeScanMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScanMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScanMaxTimeSecondsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScanMaxTimeSecondsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeScanMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeScanMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeScanMaxTimeSecondsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeConnectMaxTimeSecondsWithCompletionHandler:@
readAttributeConnectMaxTimeSecondsWithCompletionHandlerSelector :: Selector
readAttributeConnectMaxTimeSecondsWithCompletionHandlerSelector = mkSelector "readAttributeConnectMaxTimeSecondsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeConnectMaxTimeSecondsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeConnectMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeConnectMaxTimeSecondsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeConnectMaxTimeSecondsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeConnectMaxTimeSecondsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeConnectMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeConnectMaxTimeSecondsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeConnectMaxTimeSecondsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeInterfaceEnabledWithCompletionHandler:@
readAttributeInterfaceEnabledWithCompletionHandlerSelector :: Selector
readAttributeInterfaceEnabledWithCompletionHandlerSelector = mkSelector "readAttributeInterfaceEnabledWithCompletionHandler:"

-- | @Selector@ for @writeAttributeInterfaceEnabledWithValue:completionHandler:@
writeAttributeInterfaceEnabledWithValue_completionHandlerSelector :: Selector
writeAttributeInterfaceEnabledWithValue_completionHandlerSelector = mkSelector "writeAttributeInterfaceEnabledWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeInterfaceEnabledWithValue:params:completionHandler:@
writeAttributeInterfaceEnabledWithValue_params_completionHandlerSelector :: Selector
writeAttributeInterfaceEnabledWithValue_params_completionHandlerSelector = mkSelector "writeAttributeInterfaceEnabledWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeInterfaceEnabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeInterfaceEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeInterfaceEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInterfaceEnabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInterfaceEnabledWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeInterfaceEnabledWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeInterfaceEnabledWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeInterfaceEnabledWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLastNetworkingStatusWithCompletionHandler:@
readAttributeLastNetworkingStatusWithCompletionHandlerSelector :: Selector
readAttributeLastNetworkingStatusWithCompletionHandlerSelector = mkSelector "readAttributeLastNetworkingStatusWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeLastNetworkingStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLastNetworkingStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLastNetworkingStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLastNetworkingStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLastNetworkingStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLastNetworkingStatusWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLastNetworkingStatusWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLastNetworkingStatusWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLastNetworkIDWithCompletionHandler:@
readAttributeLastNetworkIDWithCompletionHandlerSelector :: Selector
readAttributeLastNetworkIDWithCompletionHandlerSelector = mkSelector "readAttributeLastNetworkIDWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeLastNetworkIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLastNetworkIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLastNetworkIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLastNetworkIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLastNetworkIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLastNetworkIDWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLastNetworkIDWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLastNetworkIDWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLastConnectErrorValueWithCompletionHandler:@
readAttributeLastConnectErrorValueWithCompletionHandlerSelector :: Selector
readAttributeLastConnectErrorValueWithCompletionHandlerSelector = mkSelector "readAttributeLastConnectErrorValueWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeLastConnectErrorValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLastConnectErrorValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLastConnectErrorValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLastConnectErrorValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLastConnectErrorValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLastConnectErrorValueWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLastConnectErrorValueWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLastConnectErrorValueWithAttributeCache:endpoint:queue:completionHandler:"

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

