{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Zone Management
--
-- This cluster provides an interface to manage regions of interest, or Zones, which can be either manufacturer or user defined.
--
-- Generated bindings for @MTRBaseClusterZoneManagement@.
module ObjC.Matter.MTRBaseClusterZoneManagement
  ( MTRBaseClusterZoneManagement
  , IsMTRBaseClusterZoneManagement(..)
  , createTwoDCartesianZoneWithParams_completion
  , updateTwoDCartesianZoneWithParams_completion
  , removeZoneWithParams_completion
  , createOrUpdateTriggerWithParams_completion
  , removeTriggerWithParams_completion
  , readAttributeMaxUserDefinedZonesWithCompletion
  , subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxZonesWithCompletion
  , subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completion
  , readAttributeZonesWithCompletion
  , subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandler
  , readAttributeZonesWithClusterStateCache_endpoint_queue_completion
  , readAttributeTriggersWithCompletion
  , subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandler
  , readAttributeTriggersWithClusterStateCache_endpoint_queue_completion
  , readAttributeSensitivityMaxWithCompletion
  , subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completion
  , readAttributeSensitivityWithCompletion
  , writeAttributeSensitivityWithValue_completion
  , writeAttributeSensitivityWithValue_params_completion
  , subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandler
  , readAttributeSensitivityWithClusterStateCache_endpoint_queue_completion
  , readAttributeTwoDCartesianMaxWithCompletion
  , subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completion
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
  , createTwoDCartesianZoneWithParams_completionSelector
  , updateTwoDCartesianZoneWithParams_completionSelector
  , removeZoneWithParams_completionSelector
  , createOrUpdateTriggerWithParams_completionSelector
  , removeTriggerWithParams_completionSelector
  , readAttributeMaxUserDefinedZonesWithCompletionSelector
  , subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxZonesWithCompletionSelector
  , subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeZonesWithCompletionSelector
  , subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeZonesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTriggersWithCompletionSelector
  , subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTriggersWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSensitivityMaxWithCompletionSelector
  , subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSensitivityWithCompletionSelector
  , writeAttributeSensitivityWithValue_completionSelector
  , writeAttributeSensitivityWithValue_params_completionSelector
  , subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSensitivityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTwoDCartesianMaxWithCompletionSelector
  , subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command CreateTwoDCartesianZone
--
-- This command SHALL create and store a TwoD Cartesian Zone.
--
-- ObjC selector: @- createTwoDCartesianZoneWithParams:completion:@
createTwoDCartesianZoneWithParams_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRZoneManagementClusterCreateTwoDCartesianZoneParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> IO ()
createTwoDCartesianZoneWithParams_completion mtrBaseClusterZoneManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "createTwoDCartesianZoneWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command UpdateTwoDCartesianZone
--
-- The UpdateTwoDCartesianZone SHALL update a stored TwoD Cartesian Zone.
--
-- ObjC selector: @- updateTwoDCartesianZoneWithParams:completion:@
updateTwoDCartesianZoneWithParams_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> IO ()
updateTwoDCartesianZoneWithParams_completion mtrBaseClusterZoneManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "updateTwoDCartesianZoneWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveZone
--
-- This command SHALL remove the user-defined Zone indicated by ZoneID.
--
-- ObjC selector: @- removeZoneWithParams:completion:@
removeZoneWithParams_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRZoneManagementClusterRemoveZoneParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> IO ()
removeZoneWithParams_completion mtrBaseClusterZoneManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "removeZoneWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command CreateOrUpdateTrigger
--
-- This command is used to create or update a Trigger for the specified motion Zone.
--
-- ObjC selector: @- createOrUpdateTriggerWithParams:completion:@
createOrUpdateTriggerWithParams_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRZoneManagementClusterCreateOrUpdateTriggerParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> IO ()
createOrUpdateTriggerWithParams_completion mtrBaseClusterZoneManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "createOrUpdateTriggerWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveTrigger
--
-- This command SHALL remove the Trigger for the provided ZoneID.
--
-- ObjC selector: @- removeTriggerWithParams:completion:@
removeTriggerWithParams_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRZoneManagementClusterRemoveTriggerParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> IO ()
removeTriggerWithParams_completion mtrBaseClusterZoneManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "removeTriggerWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxUserDefinedZonesWithCompletion:@
readAttributeMaxUserDefinedZonesWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeMaxUserDefinedZonesWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeMaxUserDefinedZonesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxUserDefinedZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeMaxUserDefinedZonesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxUserDefinedZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxUserDefinedZonesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxZonesWithCompletion:@
readAttributeMaxZonesWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeMaxZonesWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeMaxZonesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeMaxZonesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxZonesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeZonesWithCompletion:@
readAttributeZonesWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeZonesWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeZonesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeZonesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeZonesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeZonesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeZonesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTriggersWithCompletion:@
readAttributeTriggersWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeTriggersWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeTriggersWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTriggersWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeTriggersWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTriggersWithClusterStateCache:endpoint:queue:completion:@
readAttributeTriggersWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTriggersWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTriggersWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSensitivityMaxWithCompletion:@
readAttributeSensitivityMaxWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeSensitivityMaxWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeSensitivityMaxWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSensitivityMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeSensitivityMaxWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSensitivityMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSensitivityMaxWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSensitivityWithCompletion:@
readAttributeSensitivityWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeSensitivityWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeSensitivityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSensitivityWithValue:completion:@
writeAttributeSensitivityWithValue_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsNSNumber value) => mtrBaseClusterZoneManagement -> value -> Ptr () -> IO ()
writeAttributeSensitivityWithValue_completion mtrBaseClusterZoneManagement  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "writeAttributeSensitivityWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSensitivityWithValue:params:completion:@
writeAttributeSensitivityWithValue_params_completion :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterZoneManagement -> value -> params -> Ptr () -> IO ()
writeAttributeSensitivityWithValue_params_completion mtrBaseClusterZoneManagement  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterZoneManagement (mkSelector "writeAttributeSensitivityWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSensitivityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeSensitivityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSensitivityWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensitivityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSensitivityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSensitivityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTwoDCartesianMaxWithCompletion:@
readAttributeTwoDCartesianMaxWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeTwoDCartesianMaxWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeTwoDCartesianMaxWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTwoDCartesianMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeTwoDCartesianMaxWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTwoDCartesianMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTwoDCartesianMaxWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterZoneManagement  completion =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRSubscribeParams params) => mtrBaseClusterZoneManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterZoneManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterZoneManagement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement => mtrBaseClusterZoneManagement -> IO (Id MTRBaseClusterZoneManagement)
init_ mtrBaseClusterZoneManagement  =
    sendMsg mtrBaseClusterZoneManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterZoneManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterZoneManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterZoneManagement mtrBaseClusterZoneManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterZoneManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterZoneManagement)
initWithDevice_endpointID_queue mtrBaseClusterZoneManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterZoneManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createTwoDCartesianZoneWithParams:completion:@
createTwoDCartesianZoneWithParams_completionSelector :: Selector
createTwoDCartesianZoneWithParams_completionSelector = mkSelector "createTwoDCartesianZoneWithParams:completion:"

-- | @Selector@ for @updateTwoDCartesianZoneWithParams:completion:@
updateTwoDCartesianZoneWithParams_completionSelector :: Selector
updateTwoDCartesianZoneWithParams_completionSelector = mkSelector "updateTwoDCartesianZoneWithParams:completion:"

-- | @Selector@ for @removeZoneWithParams:completion:@
removeZoneWithParams_completionSelector :: Selector
removeZoneWithParams_completionSelector = mkSelector "removeZoneWithParams:completion:"

-- | @Selector@ for @createOrUpdateTriggerWithParams:completion:@
createOrUpdateTriggerWithParams_completionSelector :: Selector
createOrUpdateTriggerWithParams_completionSelector = mkSelector "createOrUpdateTriggerWithParams:completion:"

-- | @Selector@ for @removeTriggerWithParams:completion:@
removeTriggerWithParams_completionSelector :: Selector
removeTriggerWithParams_completionSelector = mkSelector "removeTriggerWithParams:completion:"

-- | @Selector@ for @readAttributeMaxUserDefinedZonesWithCompletion:@
readAttributeMaxUserDefinedZonesWithCompletionSelector :: Selector
readAttributeMaxUserDefinedZonesWithCompletionSelector = mkSelector "readAttributeMaxUserDefinedZonesWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxUserDefinedZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxUserDefinedZonesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxUserDefinedZonesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxUserDefinedZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxUserDefinedZonesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxUserDefinedZonesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxZonesWithCompletion:@
readAttributeMaxZonesWithCompletionSelector :: Selector
readAttributeMaxZonesWithCompletionSelector = mkSelector "readAttributeMaxZonesWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxZonesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxZonesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxZonesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxZonesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeZonesWithCompletion:@
readAttributeZonesWithCompletionSelector :: Selector
readAttributeZonesWithCompletionSelector = mkSelector "readAttributeZonesWithCompletion:"

-- | @Selector@ for @subscribeAttributeZonesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeZonesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeZonesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeZonesWithClusterStateCache:endpoint:queue:completion:@
readAttributeZonesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeZonesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeZonesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTriggersWithCompletion:@
readAttributeTriggersWithCompletionSelector :: Selector
readAttributeTriggersWithCompletionSelector = mkSelector "readAttributeTriggersWithCompletion:"

-- | @Selector@ for @subscribeAttributeTriggersWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTriggersWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTriggersWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTriggersWithClusterStateCache:endpoint:queue:completion:@
readAttributeTriggersWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTriggersWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTriggersWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSensitivityMaxWithCompletion:@
readAttributeSensitivityMaxWithCompletionSelector :: Selector
readAttributeSensitivityMaxWithCompletionSelector = mkSelector "readAttributeSensitivityMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributeSensitivityMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSensitivityMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSensitivityMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSensitivityMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSensitivityMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSensitivityMaxWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSensitivityWithCompletion:@
readAttributeSensitivityWithCompletionSelector :: Selector
readAttributeSensitivityWithCompletionSelector = mkSelector "readAttributeSensitivityWithCompletion:"

-- | @Selector@ for @writeAttributeSensitivityWithValue:completion:@
writeAttributeSensitivityWithValue_completionSelector :: Selector
writeAttributeSensitivityWithValue_completionSelector = mkSelector "writeAttributeSensitivityWithValue:completion:"

-- | @Selector@ for @writeAttributeSensitivityWithValue:params:completion:@
writeAttributeSensitivityWithValue_params_completionSelector :: Selector
writeAttributeSensitivityWithValue_params_completionSelector = mkSelector "writeAttributeSensitivityWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSensitivityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSensitivityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSensitivityWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensitivityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSensitivityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSensitivityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTwoDCartesianMaxWithCompletion:@
readAttributeTwoDCartesianMaxWithCompletionSelector :: Selector
readAttributeTwoDCartesianMaxWithCompletionSelector = mkSelector "readAttributeTwoDCartesianMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributeTwoDCartesianMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTwoDCartesianMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTwoDCartesianMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTwoDCartesianMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTwoDCartesianMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTwoDCartesianMaxWithClusterStateCache:endpoint:queue:completion:"

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

