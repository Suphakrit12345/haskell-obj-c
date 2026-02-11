{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Grid Conditions
--
-- The Electrical Grid Conditions Cluster provides the mechanism for communicating electricity grid carbon intensity to devices within the premises in units of Grams of CO2e per kWh.
--
-- Generated bindings for @MTRBaseClusterElectricalGridConditions@.
module ObjC.Matter.MTRBaseClusterElectricalGridConditions
  ( MTRBaseClusterElectricalGridConditions
  , IsMTRBaseClusterElectricalGridConditions(..)
  , readAttributeLocalGenerationAvailableWithCompletion
  , writeAttributeLocalGenerationAvailableWithValue_completion
  , writeAttributeLocalGenerationAvailableWithValue_params_completion
  , subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandler
  , readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentConditionsWithCompletion
  , subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completion
  , readAttributeForecastConditionsWithCompletion
  , subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandler
  , readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeLocalGenerationAvailableWithCompletionSelector
  , writeAttributeLocalGenerationAvailableWithValue_completionSelector
  , writeAttributeLocalGenerationAvailableWithValue_params_completionSelector
  , subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentConditionsWithCompletionSelector
  , subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeForecastConditionsWithCompletionSelector
  , subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributeLocalGenerationAvailableWithCompletion:@
readAttributeLocalGenerationAvailableWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeLocalGenerationAvailableWithCompletion mtrBaseClusterElectricalGridConditions  completion =
    sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "readAttributeLocalGenerationAvailableWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLocalGenerationAvailableWithValue:completion:@
writeAttributeLocalGenerationAvailableWithValue_completion :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsNSNumber value) => mtrBaseClusterElectricalGridConditions -> value -> Ptr () -> IO ()
writeAttributeLocalGenerationAvailableWithValue_completion mtrBaseClusterElectricalGridConditions  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "writeAttributeLocalGenerationAvailableWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLocalGenerationAvailableWithValue:params:completion:@
writeAttributeLocalGenerationAvailableWithValue_params_completion :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterElectricalGridConditions -> value -> params -> Ptr () -> IO ()
writeAttributeLocalGenerationAvailableWithValue_params_completion mtrBaseClusterElectricalGridConditions  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "writeAttributeLocalGenerationAvailableWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLocalGenerationAvailableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "subscribeAttributeLocalGenerationAvailableWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLocalGenerationAvailableWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLocalGenerationAvailableWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentConditionsWithCompletion:@
readAttributeCurrentConditionsWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeCurrentConditionsWithCompletion mtrBaseClusterElectricalGridConditions  completion =
    sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "readAttributeCurrentConditionsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentConditionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "subscribeAttributeCurrentConditionsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentConditionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentConditionsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeForecastConditionsWithCompletion:@
readAttributeForecastConditionsWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeForecastConditionsWithCompletion mtrBaseClusterElectricalGridConditions  completion =
    sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "readAttributeForecastConditionsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeForecastConditionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "subscribeAttributeForecastConditionsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeForecastConditionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeForecastConditionsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterElectricalGridConditions  completion =
    sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterElectricalGridConditions  completion =
    sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterElectricalGridConditions  completion =
    sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterElectricalGridConditions  completion =
    sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterElectricalGridConditions  completion =
    sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRSubscribeParams params) => mtrBaseClusterElectricalGridConditions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalGridConditions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions => mtrBaseClusterElectricalGridConditions -> IO (Id MTRBaseClusterElectricalGridConditions)
init_ mtrBaseClusterElectricalGridConditions  =
    sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterElectricalGridConditions)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalGridConditions"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterElectricalGridConditions mtrBaseClusterElectricalGridConditions, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterElectricalGridConditions -> device -> endpointID -> queue -> IO (Id MTRBaseClusterElectricalGridConditions)
initWithDevice_endpointID_queue mtrBaseClusterElectricalGridConditions  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterElectricalGridConditions (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeLocalGenerationAvailableWithCompletion:@
readAttributeLocalGenerationAvailableWithCompletionSelector :: Selector
readAttributeLocalGenerationAvailableWithCompletionSelector = mkSelector "readAttributeLocalGenerationAvailableWithCompletion:"

-- | @Selector@ for @writeAttributeLocalGenerationAvailableWithValue:completion:@
writeAttributeLocalGenerationAvailableWithValue_completionSelector :: Selector
writeAttributeLocalGenerationAvailableWithValue_completionSelector = mkSelector "writeAttributeLocalGenerationAvailableWithValue:completion:"

-- | @Selector@ for @writeAttributeLocalGenerationAvailableWithValue:params:completion:@
writeAttributeLocalGenerationAvailableWithValue_params_completionSelector :: Selector
writeAttributeLocalGenerationAvailableWithValue_params_completionSelector = mkSelector "writeAttributeLocalGenerationAvailableWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLocalGenerationAvailableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLocalGenerationAvailableWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocalGenerationAvailableWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocalGenerationAvailableWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLocalGenerationAvailableWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLocalGenerationAvailableWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentConditionsWithCompletion:@
readAttributeCurrentConditionsWithCompletionSelector :: Selector
readAttributeCurrentConditionsWithCompletionSelector = mkSelector "readAttributeCurrentConditionsWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentConditionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentConditionsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentConditionsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentConditionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentConditionsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentConditionsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeForecastConditionsWithCompletion:@
readAttributeForecastConditionsWithCompletionSelector :: Selector
readAttributeForecastConditionsWithCompletionSelector = mkSelector "readAttributeForecastConditionsWithCompletion:"

-- | @Selector@ for @subscribeAttributeForecastConditionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeForecastConditionsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeForecastConditionsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeForecastConditionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeForecastConditionsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeForecastConditionsWithClusterStateCache:endpoint:queue:completion:"

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

