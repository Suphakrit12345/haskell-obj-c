{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Refrigerator And Temperature Controlled Cabinet Mode
--
-- Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode@.
module ObjC.Matter.MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode
  ( MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode
  , IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode(..)
  , changeToModeWithParams_completion
  , readAttributeSupportedModesWithCompletion
  , subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentModeWithCompletion
  , subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion
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
  , changeToModeWithParams_completionSelector
  , readAttributeSupportedModesWithCompletionSelector
  , subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentModeWithCompletionSelector
  , subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command ChangeToMode
--
-- This command is used to change device modes.        On receipt of this command the device SHALL respond with a ChangeToModeResponse command.
--
-- ObjC selector: @- changeToModeWithParams:completion:@
changeToModeWithParams_completion :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> IO ()
changeToModeWithParams_completion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "changeToModeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedModesWithCompletion:@
readAttributeSupportedModesWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeSupportedModesWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  completion =
    sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeSupportedModesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentModeWithCompletion:@
readAttributeCurrentModeWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeCurrentModeWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  completion =
    sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeCurrentModeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  completion =
    sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  completion =
    sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  completion =
    sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  completion =
    sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  completion =
    sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> IO (Id MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode)
init_ mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  =
    sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode -> device -> endpointID -> queue -> IO (Id MTRBaseClusterRefrigeratorAndTemperatureControlledCabinetMode)
initWithDevice_endpointID_queue mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:completion:@
changeToModeWithParams_completionSelector :: Selector
changeToModeWithParams_completionSelector = mkSelector "changeToModeWithParams:completion:"

-- | @Selector@ for @readAttributeSupportedModesWithCompletion:@
readAttributeSupportedModesWithCompletionSelector :: Selector
readAttributeSupportedModesWithCompletionSelector = mkSelector "readAttributeSupportedModesWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentModeWithCompletion:@
readAttributeCurrentModeWithCompletionSelector :: Selector
readAttributeCurrentModeWithCompletionSelector = mkSelector "readAttributeCurrentModeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:"

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

