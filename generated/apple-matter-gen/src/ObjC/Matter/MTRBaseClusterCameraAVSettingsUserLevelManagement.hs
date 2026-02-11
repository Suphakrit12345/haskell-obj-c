{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Camera AV Settings User Level Management
--
-- This cluster provides an interface into controls associated with the operation of a device that provides pan, tilt, and zoom functions, either mechanically, or against a digital image.
--
-- Generated bindings for @MTRBaseClusterCameraAVSettingsUserLevelManagement@.
module ObjC.Matter.MTRBaseClusterCameraAVSettingsUserLevelManagement
  ( MTRBaseClusterCameraAVSettingsUserLevelManagement
  , IsMTRBaseClusterCameraAVSettingsUserLevelManagement(..)
  , mptzSetPositionWithParams_completion
  , mptzSetPositionWithCompletion
  , mptzRelativeMoveWithParams_completion
  , mptzRelativeMoveWithCompletion
  , mptzMoveToPresetWithParams_completion
  , mptzSavePresetWithParams_completion
  , mptzRemovePresetWithParams_completion
  , dptzSetViewportWithParams_completion
  , dptzRelativeMoveWithParams_completion
  , readAttributeMPTZPositionWithCompletion
  , subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandler
  , readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxPresetsWithCompletion
  , subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completion
  , readAttributeMPTZPresetsWithCompletion
  , subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandler
  , readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completion
  , readAttributeDPTZStreamsWithCompletion
  , subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandler
  , readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completion
  , readAttributeZoomMaxWithCompletion
  , subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completion
  , readAttributeTiltMinWithCompletion
  , subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandler
  , readAttributeTiltMinWithClusterStateCache_endpoint_queue_completion
  , readAttributeTiltMaxWithCompletion
  , subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completion
  , readAttributePanMinWithCompletion
  , subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandler
  , readAttributePanMinWithClusterStateCache_endpoint_queue_completion
  , readAttributePanMaxWithCompletion
  , subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributePanMaxWithClusterStateCache_endpoint_queue_completion
  , readAttributeMovementStateWithCompletion
  , subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeMovementStateWithClusterStateCache_endpoint_queue_completion
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
  , mptzSetPositionWithParams_completionSelector
  , mptzSetPositionWithCompletionSelector
  , mptzRelativeMoveWithParams_completionSelector
  , mptzRelativeMoveWithCompletionSelector
  , mptzMoveToPresetWithParams_completionSelector
  , mptzSavePresetWithParams_completionSelector
  , mptzRemovePresetWithParams_completionSelector
  , dptzSetViewportWithParams_completionSelector
  , dptzRelativeMoveWithParams_completionSelector
  , readAttributeMPTZPositionWithCompletionSelector
  , subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxPresetsWithCompletionSelector
  , subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMPTZPresetsWithCompletionSelector
  , subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDPTZStreamsWithCompletionSelector
  , subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeZoomMaxWithCompletionSelector
  , subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTiltMinWithCompletionSelector
  , subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTiltMinWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTiltMaxWithCompletionSelector
  , subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePanMinWithCompletionSelector
  , subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePanMinWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePanMaxWithCompletionSelector
  , subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePanMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMovementStateWithCompletionSelector
  , subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMovementStateWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command MPTZSetPosition
--
-- This command SHALL move the camera to the provided values for pan, tilt, and zoom in the mechanical PTZ.
--
-- ObjC selector: @- MPTZSetPositionWithParams:completion:@
mptzSetPositionWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
mptzSetPositionWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZSetPositionWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- MPTZSetPositionWithCompletion:@
mptzSetPositionWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
mptzSetPositionWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZSetPositionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command MPTZRelativeMove
--
-- This command SHALL move the camera by the delta values relative to the currently defined position.
--
-- ObjC selector: @- MPTZRelativeMoveWithParams:completion:@
mptzRelativeMoveWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
mptzRelativeMoveWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZRelativeMoveWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- MPTZRelativeMoveWithCompletion:@
mptzRelativeMoveWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
mptzRelativeMoveWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZRelativeMoveWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command MPTZMoveToPreset
--
-- This command SHALL move the camera to the positions specified by the Preset passed.
--
-- ObjC selector: @- MPTZMoveToPresetWithParams:completion:@
mptzMoveToPresetWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
mptzMoveToPresetWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZMoveToPresetWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command MPTZSavePreset
--
-- This command allows creating a new preset or updating the values of an existing one.
--
-- ObjC selector: @- MPTZSavePresetWithParams:completion:@
mptzSavePresetWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
mptzSavePresetWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZSavePresetWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command MPTZRemovePreset
--
-- This command SHALL remove a preset entry from the PresetMptzTable.
--
-- ObjC selector: @- MPTZRemovePresetWithParams:completion:@
mptzRemovePresetWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRemovePresetParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
mptzRemovePresetWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZRemovePresetWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command DPTZSetViewport
--
-- This command allows for setting the digital viewport for a specific Video Stream.
--
-- ObjC selector: @- DPTZSetViewportWithParams:completion:@
dptzSetViewportWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
dptzSetViewportWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "DPTZSetViewportWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command DPTZRelativeMove
--
-- This command SHALL change the per stream viewport by the amount specified in a relative fashion.
--
-- ObjC selector: @- DPTZRelativeMoveWithParams:completion:@
dptzRelativeMoveWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
dptzRelativeMoveWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "DPTZRelativeMoveWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMPTZPositionWithCompletion:@
readAttributeMPTZPositionWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeMPTZPositionWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeMPTZPositionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMPTZPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeMPTZPositionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMPTZPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMPTZPositionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxPresetsWithCompletion:@
readAttributeMaxPresetsWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeMaxPresetsWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeMaxPresetsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxPresetsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeMaxPresetsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxPresetsWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxPresetsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMPTZPresetsWithCompletion:@
readAttributeMPTZPresetsWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeMPTZPresetsWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeMPTZPresetsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMPTZPresetsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeMPTZPresetsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMPTZPresetsWithClusterStateCache:endpoint:queue:completion:@
readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMPTZPresetsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDPTZStreamsWithCompletion:@
readAttributeDPTZStreamsWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeDPTZStreamsWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeDPTZStreamsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDPTZStreamsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeDPTZStreamsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDPTZStreamsWithClusterStateCache:endpoint:queue:completion:@
readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDPTZStreamsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeZoomMaxWithCompletion:@
readAttributeZoomMaxWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeZoomMaxWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeZoomMaxWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeZoomMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeZoomMaxWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeZoomMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeZoomMaxWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTiltMinWithCompletion:@
readAttributeTiltMinWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeTiltMinWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeTiltMinWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTiltMinWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeTiltMinWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTiltMinWithClusterStateCache:endpoint:queue:completion:@
readAttributeTiltMinWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTiltMinWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTiltMinWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTiltMaxWithCompletion:@
readAttributeTiltMaxWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeTiltMaxWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeTiltMaxWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTiltMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeTiltMaxWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTiltMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTiltMaxWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePanMinWithCompletion:@
readAttributePanMinWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributePanMinWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributePanMinWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePanMinWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributePanMinWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePanMinWithClusterStateCache:endpoint:queue:completion:@
readAttributePanMinWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePanMinWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePanMinWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePanMaxWithCompletion:@
readAttributePanMaxWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributePanMaxWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributePanMaxWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePanMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributePanMaxWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePanMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributePanMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePanMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePanMaxWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMovementStateWithCompletion:@
readAttributeMovementStateWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeMovementStateWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeMovementStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMovementStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeMovementStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMovementStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeMovementStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMovementStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMovementStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement  completion =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> IO (Id MTRBaseClusterCameraAVSettingsUserLevelManagement)
init_ mtrBaseClusterCameraAVSettingsUserLevelManagement  =
    sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterCameraAVSettingsUserLevelManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterCameraAVSettingsUserLevelManagement)
initWithDevice_endpointID_queue mtrBaseClusterCameraAVSettingsUserLevelManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterCameraAVSettingsUserLevelManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @MPTZSetPositionWithParams:completion:@
mptzSetPositionWithParams_completionSelector :: Selector
mptzSetPositionWithParams_completionSelector = mkSelector "MPTZSetPositionWithParams:completion:"

-- | @Selector@ for @MPTZSetPositionWithCompletion:@
mptzSetPositionWithCompletionSelector :: Selector
mptzSetPositionWithCompletionSelector = mkSelector "MPTZSetPositionWithCompletion:"

-- | @Selector@ for @MPTZRelativeMoveWithParams:completion:@
mptzRelativeMoveWithParams_completionSelector :: Selector
mptzRelativeMoveWithParams_completionSelector = mkSelector "MPTZRelativeMoveWithParams:completion:"

-- | @Selector@ for @MPTZRelativeMoveWithCompletion:@
mptzRelativeMoveWithCompletionSelector :: Selector
mptzRelativeMoveWithCompletionSelector = mkSelector "MPTZRelativeMoveWithCompletion:"

-- | @Selector@ for @MPTZMoveToPresetWithParams:completion:@
mptzMoveToPresetWithParams_completionSelector :: Selector
mptzMoveToPresetWithParams_completionSelector = mkSelector "MPTZMoveToPresetWithParams:completion:"

-- | @Selector@ for @MPTZSavePresetWithParams:completion:@
mptzSavePresetWithParams_completionSelector :: Selector
mptzSavePresetWithParams_completionSelector = mkSelector "MPTZSavePresetWithParams:completion:"

-- | @Selector@ for @MPTZRemovePresetWithParams:completion:@
mptzRemovePresetWithParams_completionSelector :: Selector
mptzRemovePresetWithParams_completionSelector = mkSelector "MPTZRemovePresetWithParams:completion:"

-- | @Selector@ for @DPTZSetViewportWithParams:completion:@
dptzSetViewportWithParams_completionSelector :: Selector
dptzSetViewportWithParams_completionSelector = mkSelector "DPTZSetViewportWithParams:completion:"

-- | @Selector@ for @DPTZRelativeMoveWithParams:completion:@
dptzRelativeMoveWithParams_completionSelector :: Selector
dptzRelativeMoveWithParams_completionSelector = mkSelector "DPTZRelativeMoveWithParams:completion:"

-- | @Selector@ for @readAttributeMPTZPositionWithCompletion:@
readAttributeMPTZPositionWithCompletionSelector :: Selector
readAttributeMPTZPositionWithCompletionSelector = mkSelector "readAttributeMPTZPositionWithCompletion:"

-- | @Selector@ for @subscribeAttributeMPTZPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMPTZPositionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMPTZPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMPTZPositionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxPresetsWithCompletion:@
readAttributeMaxPresetsWithCompletionSelector :: Selector
readAttributeMaxPresetsWithCompletionSelector = mkSelector "readAttributeMaxPresetsWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxPresetsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxPresetsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxPresetsWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxPresetsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMPTZPresetsWithCompletion:@
readAttributeMPTZPresetsWithCompletionSelector :: Selector
readAttributeMPTZPresetsWithCompletionSelector = mkSelector "readAttributeMPTZPresetsWithCompletion:"

-- | @Selector@ for @subscribeAttributeMPTZPresetsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMPTZPresetsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMPTZPresetsWithClusterStateCache:endpoint:queue:completion:@
readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMPTZPresetsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDPTZStreamsWithCompletion:@
readAttributeDPTZStreamsWithCompletionSelector :: Selector
readAttributeDPTZStreamsWithCompletionSelector = mkSelector "readAttributeDPTZStreamsWithCompletion:"

-- | @Selector@ for @subscribeAttributeDPTZStreamsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDPTZStreamsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDPTZStreamsWithClusterStateCache:endpoint:queue:completion:@
readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDPTZStreamsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeZoomMaxWithCompletion:@
readAttributeZoomMaxWithCompletionSelector :: Selector
readAttributeZoomMaxWithCompletionSelector = mkSelector "readAttributeZoomMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributeZoomMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeZoomMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeZoomMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeZoomMaxWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTiltMinWithCompletion:@
readAttributeTiltMinWithCompletionSelector :: Selector
readAttributeTiltMinWithCompletionSelector = mkSelector "readAttributeTiltMinWithCompletion:"

-- | @Selector@ for @subscribeAttributeTiltMinWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTiltMinWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTiltMinWithClusterStateCache:endpoint:queue:completion:@
readAttributeTiltMinWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTiltMinWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTiltMinWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTiltMaxWithCompletion:@
readAttributeTiltMaxWithCompletionSelector :: Selector
readAttributeTiltMaxWithCompletionSelector = mkSelector "readAttributeTiltMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributeTiltMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTiltMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTiltMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTiltMaxWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePanMinWithCompletion:@
readAttributePanMinWithCompletionSelector :: Selector
readAttributePanMinWithCompletionSelector = mkSelector "readAttributePanMinWithCompletion:"

-- | @Selector@ for @subscribeAttributePanMinWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePanMinWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePanMinWithClusterStateCache:endpoint:queue:completion:@
readAttributePanMinWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePanMinWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePanMinWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePanMaxWithCompletion:@
readAttributePanMaxWithCompletionSelector :: Selector
readAttributePanMaxWithCompletionSelector = mkSelector "readAttributePanMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributePanMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePanMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePanMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributePanMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePanMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePanMaxWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMovementStateWithCompletion:@
readAttributeMovementStateWithCompletionSelector :: Selector
readAttributeMovementStateWithCompletionSelector = mkSelector "readAttributeMovementStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeMovementStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMovementStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMovementStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeMovementStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMovementStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMovementStateWithClusterStateCache:endpoint:queue:completion:"

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

