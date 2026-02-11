{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Fan Control
--
-- An interface for controlling a fan in a heating/cooling system.
--
-- Generated bindings for @MTRBaseClusterFanControl@.
module ObjC.Matter.MTRBaseClusterFanControl
  ( MTRBaseClusterFanControl
  , IsMTRBaseClusterFanControl(..)
  , stepWithParams_completion
  , readAttributeFanModeWithCompletion
  , writeAttributeFanModeWithValue_completion
  , writeAttributeFanModeWithValue_params_completion
  , subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeFanModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeFanModeSequenceWithCompletion
  , writeAttributeFanModeSequenceWithValue_completion
  , writeAttributeFanModeSequenceWithValue_params_completion
  , subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandler
  , readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completion
  , readAttributePercentSettingWithCompletion
  , writeAttributePercentSettingWithValue_completion
  , writeAttributePercentSettingWithValue_params_completion
  , subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandler
  , readAttributePercentSettingWithClusterStateCache_endpoint_queue_completion
  , readAttributePercentCurrentWithCompletion
  , subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeSpeedMaxWithCompletion
  , subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completion
  , readAttributeSpeedSettingWithCompletion
  , writeAttributeSpeedSettingWithValue_completion
  , writeAttributeSpeedSettingWithValue_params_completion
  , subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completion
  , readAttributeSpeedCurrentWithCompletion
  , subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeRockSupportWithCompletion
  , subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandler
  , readAttributeRockSupportWithClusterStateCache_endpoint_queue_completion
  , readAttributeRockSettingWithCompletion
  , writeAttributeRockSettingWithValue_completion
  , writeAttributeRockSettingWithValue_params_completion
  , subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandler
  , readAttributeRockSettingWithClusterStateCache_endpoint_queue_completion
  , readAttributeWindSupportWithCompletion
  , subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandler
  , readAttributeWindSupportWithClusterStateCache_endpoint_queue_completion
  , readAttributeWindSettingWithCompletion
  , writeAttributeWindSettingWithValue_completion
  , writeAttributeWindSettingWithValue_params_completion
  , subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandler
  , readAttributeWindSettingWithClusterStateCache_endpoint_queue_completion
  , readAttributeAirflowDirectionWithCompletion
  , writeAttributeAirflowDirectionWithValue_completion
  , writeAttributeAirflowDirectionWithValue_params_completion
  , subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandler
  , readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeFanModeWithCompletionHandler
  , writeAttributeFanModeWithValue_completionHandler
  , writeAttributeFanModeWithValue_params_completionHandler
  , subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFanModeSequenceWithCompletionHandler
  , writeAttributeFanModeSequenceWithValue_completionHandler
  , writeAttributeFanModeSequenceWithValue_params_completionHandler
  , subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePercentSettingWithCompletionHandler
  , writeAttributePercentSettingWithValue_completionHandler
  , writeAttributePercentSettingWithValue_params_completionHandler
  , subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePercentCurrentWithCompletionHandler
  , subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSpeedMaxWithCompletionHandler
  , subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSpeedSettingWithCompletionHandler
  , writeAttributeSpeedSettingWithValue_completionHandler
  , writeAttributeSpeedSettingWithValue_params_completionHandler
  , subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSpeedCurrentWithCompletionHandler
  , subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeRockSupportWithCompletionHandler
  , subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeRockSettingWithCompletionHandler
  , writeAttributeRockSettingWithValue_completionHandler
  , writeAttributeRockSettingWithValue_params_completionHandler
  , subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeWindSupportWithCompletionHandler
  , subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeWindSettingWithCompletionHandler
  , writeAttributeWindSettingWithValue_completionHandler
  , writeAttributeWindSettingWithValue_params_completionHandler
  , subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandler
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
  , stepWithParams_completionSelector
  , readAttributeFanModeWithCompletionSelector
  , writeAttributeFanModeWithValue_completionSelector
  , writeAttributeFanModeWithValue_params_completionSelector
  , subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeFanModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFanModeSequenceWithCompletionSelector
  , writeAttributeFanModeSequenceWithValue_completionSelector
  , writeAttributeFanModeSequenceWithValue_params_completionSelector
  , subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePercentSettingWithCompletionSelector
  , writeAttributePercentSettingWithValue_completionSelector
  , writeAttributePercentSettingWithValue_params_completionSelector
  , subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePercentSettingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePercentCurrentWithCompletionSelector
  , subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSpeedMaxWithCompletionSelector
  , subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSpeedSettingWithCompletionSelector
  , writeAttributeSpeedSettingWithValue_completionSelector
  , writeAttributeSpeedSettingWithValue_params_completionSelector
  , subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSpeedCurrentWithCompletionSelector
  , subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRockSupportWithCompletionSelector
  , subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRockSupportWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRockSettingWithCompletionSelector
  , writeAttributeRockSettingWithValue_completionSelector
  , writeAttributeRockSettingWithValue_params_completionSelector
  , subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRockSettingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeWindSupportWithCompletionSelector
  , subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeWindSupportWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeWindSettingWithCompletionSelector
  , writeAttributeWindSettingWithValue_completionSelector
  , writeAttributeWindSettingWithValue_params_completionSelector
  , subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeWindSettingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAirflowDirectionWithCompletionSelector
  , writeAttributeAirflowDirectionWithValue_completionSelector
  , writeAttributeAirflowDirectionWithValue_params_completionSelector
  , subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completionSelector
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
  , readAttributeFanModeWithCompletionHandlerSelector
  , writeAttributeFanModeWithValue_completionHandlerSelector
  , writeAttributeFanModeWithValue_params_completionHandlerSelector
  , subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFanModeSequenceWithCompletionHandlerSelector
  , writeAttributeFanModeSequenceWithValue_completionHandlerSelector
  , writeAttributeFanModeSequenceWithValue_params_completionHandlerSelector
  , subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePercentSettingWithCompletionHandlerSelector
  , writeAttributePercentSettingWithValue_completionHandlerSelector
  , writeAttributePercentSettingWithValue_params_completionHandlerSelector
  , subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePercentCurrentWithCompletionHandlerSelector
  , subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSpeedMaxWithCompletionHandlerSelector
  , subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSpeedSettingWithCompletionHandlerSelector
  , writeAttributeSpeedSettingWithValue_completionHandlerSelector
  , writeAttributeSpeedSettingWithValue_params_completionHandlerSelector
  , subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSpeedCurrentWithCompletionHandlerSelector
  , subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeRockSupportWithCompletionHandlerSelector
  , subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeRockSettingWithCompletionHandlerSelector
  , writeAttributeRockSettingWithValue_completionHandlerSelector
  , writeAttributeRockSettingWithValue_params_completionHandlerSelector
  , subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeWindSupportWithCompletionHandlerSelector
  , subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeWindSettingWithCompletionHandlerSelector
  , writeAttributeWindSettingWithValue_completionHandlerSelector
  , writeAttributeWindSettingWithValue_params_completionHandlerSelector
  , subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command Step
--
-- This command speeds up or slows down the fan, in steps, without a client having to know the fan speed.
--
-- ObjC selector: @- stepWithParams:completion:@
stepWithParams_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRFanControlClusterStepParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> IO ()
stepWithParams_completion mtrBaseClusterFanControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "stepWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFanModeWithCompletion:@
readAttributeFanModeWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFanModeWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeFanModeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeFanModeWithValue:completion:@
writeAttributeFanModeWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeFanModeWithValue_completion mtrBaseClusterFanControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeFanModeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeFanModeWithValue:params:completion:@
writeAttributeFanModeWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeFanModeWithValue_params_completion mtrBaseClusterFanControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeFanModeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFanModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeFanModeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFanModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeFanModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFanModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFanModeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFanModeSequenceWithCompletion:@
readAttributeFanModeSequenceWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFanModeSequenceWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeFanModeSequenceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeFanModeSequenceWithValue:completion:@
writeAttributeFanModeSequenceWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeFanModeSequenceWithValue_completion mtrBaseClusterFanControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeFanModeSequenceWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeFanModeSequenceWithValue:params:completion:@
writeAttributeFanModeSequenceWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeFanModeSequenceWithValue_params_completion mtrBaseClusterFanControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeFanModeSequenceWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFanModeSequenceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeFanModeSequenceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFanModeSequenceWithClusterStateCache:endpoint:queue:completion:@
readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFanModeSequenceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePercentSettingWithCompletion:@
readAttributePercentSettingWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributePercentSettingWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributePercentSettingWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributePercentSettingWithValue:completion:@
writeAttributePercentSettingWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributePercentSettingWithValue_completion mtrBaseClusterFanControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributePercentSettingWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributePercentSettingWithValue:params:completion:@
writeAttributePercentSettingWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributePercentSettingWithValue_params_completion mtrBaseClusterFanControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributePercentSettingWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePercentSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributePercentSettingWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePercentSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributePercentSettingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePercentSettingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePercentSettingWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePercentCurrentWithCompletion:@
readAttributePercentCurrentWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributePercentCurrentWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributePercentCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePercentCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributePercentCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePercentCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePercentCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSpeedMaxWithCompletion:@
readAttributeSpeedMaxWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedMaxWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeSpeedMaxWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSpeedMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeSpeedMaxWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSpeedMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSpeedMaxWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSpeedSettingWithCompletion:@
readAttributeSpeedSettingWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedSettingWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeSpeedSettingWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSpeedSettingWithValue:completion:@
writeAttributeSpeedSettingWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeSpeedSettingWithValue_completion mtrBaseClusterFanControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeSpeedSettingWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSpeedSettingWithValue:params:completion:@
writeAttributeSpeedSettingWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeSpeedSettingWithValue_params_completion mtrBaseClusterFanControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeSpeedSettingWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSpeedSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeSpeedSettingWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSpeedSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSpeedSettingWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSpeedCurrentWithCompletion:@
readAttributeSpeedCurrentWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedCurrentWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeSpeedCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSpeedCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeSpeedCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSpeedCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSpeedCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRockSupportWithCompletion:@
readAttributeRockSupportWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeRockSupportWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeRockSupportWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRockSupportWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeRockSupportWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRockSupportWithClusterStateCache:endpoint:queue:completion:@
readAttributeRockSupportWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRockSupportWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRockSupportWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRockSettingWithCompletion:@
readAttributeRockSettingWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeRockSettingWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeRockSettingWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeRockSettingWithValue:completion:@
writeAttributeRockSettingWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeRockSettingWithValue_completion mtrBaseClusterFanControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeRockSettingWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeRockSettingWithValue:params:completion:@
writeAttributeRockSettingWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeRockSettingWithValue_params_completion mtrBaseClusterFanControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeRockSettingWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRockSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeRockSettingWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRockSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeRockSettingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRockSettingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRockSettingWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeWindSupportWithCompletion:@
readAttributeWindSupportWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeWindSupportWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeWindSupportWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeWindSupportWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeWindSupportWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeWindSupportWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindSupportWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindSupportWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeWindSupportWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeWindSettingWithCompletion:@
readAttributeWindSettingWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeWindSettingWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeWindSettingWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeWindSettingWithValue:completion:@
writeAttributeWindSettingWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeWindSettingWithValue_completion mtrBaseClusterFanControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeWindSettingWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeWindSettingWithValue:params:completion:@
writeAttributeWindSettingWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeWindSettingWithValue_params_completion mtrBaseClusterFanControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeWindSettingWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeWindSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeWindSettingWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeWindSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindSettingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindSettingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeWindSettingWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAirflowDirectionWithCompletion:@
readAttributeAirflowDirectionWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeAirflowDirectionWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeAirflowDirectionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeAirflowDirectionWithValue:completion:@
writeAttributeAirflowDirectionWithValue_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeAirflowDirectionWithValue_completion mtrBaseClusterFanControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeAirflowDirectionWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeAirflowDirectionWithValue:params:completion:@
writeAttributeAirflowDirectionWithValue_params_completion :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeAirflowDirectionWithValue_params_completion mtrBaseClusterFanControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeAirflowDirectionWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAirflowDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeAirflowDirectionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAirflowDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAirflowDirectionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterFanControl  completion =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> IO (Id MTRBaseClusterFanControl)
init_ mtrBaseClusterFanControl  =
    sendMsg mtrBaseClusterFanControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterFanControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterFanControl -> device -> CUShort -> queue -> IO (Id MTRBaseClusterFanControl)
initWithDevice_endpoint_queue mtrBaseClusterFanControl  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterFanControl (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeFanModeWithCompletionHandler:@
readAttributeFanModeWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFanModeWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeFanModeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeFanModeWithValue:completionHandler:@
writeAttributeFanModeWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeFanModeWithValue_completionHandler mtrBaseClusterFanControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeFanModeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeFanModeWithValue:params:completionHandler:@
writeAttributeFanModeWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeFanModeWithValue_params_completionHandler mtrBaseClusterFanControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeFanModeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFanModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeFanModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFanModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFanModeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFanModeSequenceWithCompletionHandler:@
readAttributeFanModeSequenceWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFanModeSequenceWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeFanModeSequenceWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeFanModeSequenceWithValue:completionHandler:@
writeAttributeFanModeSequenceWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeFanModeSequenceWithValue_completionHandler mtrBaseClusterFanControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeFanModeSequenceWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeFanModeSequenceWithValue:params:completionHandler:@
writeAttributeFanModeSequenceWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeFanModeSequenceWithValue_params_completionHandler mtrBaseClusterFanControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeFanModeSequenceWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFanModeSequenceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeFanModeSequenceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFanModeSequenceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFanModeSequenceWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePercentSettingWithCompletionHandler:@
readAttributePercentSettingWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributePercentSettingWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributePercentSettingWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributePercentSettingWithValue:completionHandler:@
writeAttributePercentSettingWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributePercentSettingWithValue_completionHandler mtrBaseClusterFanControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributePercentSettingWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributePercentSettingWithValue:params:completionHandler:@
writeAttributePercentSettingWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributePercentSettingWithValue_params_completionHandler mtrBaseClusterFanControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributePercentSettingWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePercentSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributePercentSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePercentSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePercentSettingWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePercentCurrentWithCompletionHandler:@
readAttributePercentCurrentWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributePercentCurrentWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributePercentCurrentWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePercentCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributePercentCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePercentCurrentWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePercentCurrentWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSpeedMaxWithCompletionHandler:@
readAttributeSpeedMaxWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedMaxWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeSpeedMaxWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSpeedMaxWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeSpeedMaxWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSpeedMaxWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSpeedMaxWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSpeedSettingWithCompletionHandler:@
readAttributeSpeedSettingWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedSettingWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeSpeedSettingWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeSpeedSettingWithValue:completionHandler:@
writeAttributeSpeedSettingWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeSpeedSettingWithValue_completionHandler mtrBaseClusterFanControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeSpeedSettingWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeSpeedSettingWithValue:params:completionHandler:@
writeAttributeSpeedSettingWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeSpeedSettingWithValue_params_completionHandler mtrBaseClusterFanControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeSpeedSettingWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSpeedSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeSpeedSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSpeedSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSpeedSettingWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSpeedCurrentWithCompletionHandler:@
readAttributeSpeedCurrentWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeSpeedCurrentWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeSpeedCurrentWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSpeedCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeSpeedCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSpeedCurrentWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSpeedCurrentWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeRockSupportWithCompletionHandler:@
readAttributeRockSupportWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeRockSupportWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeRockSupportWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeRockSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeRockSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRockSupportWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRockSupportWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeRockSettingWithCompletionHandler:@
readAttributeRockSettingWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeRockSettingWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeRockSettingWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeRockSettingWithValue:completionHandler:@
writeAttributeRockSettingWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeRockSettingWithValue_completionHandler mtrBaseClusterFanControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeRockSettingWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeRockSettingWithValue:params:completionHandler:@
writeAttributeRockSettingWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeRockSettingWithValue_params_completionHandler mtrBaseClusterFanControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeRockSettingWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeRockSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeRockSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRockSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRockSettingWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeWindSupportWithCompletionHandler:@
readAttributeWindSupportWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeWindSupportWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeWindSupportWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeWindSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeWindSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeWindSupportWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeWindSupportWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeWindSettingWithCompletionHandler:@
readAttributeWindSettingWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeWindSettingWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeWindSettingWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeWindSettingWithValue:completionHandler:@
writeAttributeWindSettingWithValue_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value) => mtrBaseClusterFanControl -> value -> Ptr () -> IO ()
writeAttributeWindSettingWithValue_completionHandler mtrBaseClusterFanControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeWindSettingWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeWindSettingWithValue:params:completionHandler:@
writeAttributeWindSettingWithValue_params_completionHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterFanControl -> value -> params -> Ptr () -> IO ()
writeAttributeWindSettingWithValue_params_completionHandler mtrBaseClusterFanControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterFanControl (mkSelector "writeAttributeWindSettingWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeWindSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeWindSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeWindSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeWindSettingWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterFanControl mtrBaseClusterFanControl => mtrBaseClusterFanControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterFanControl  completionHandler =
    sendMsg mtrBaseClusterFanControl (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterFanControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterFanControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterFanControl (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterFanControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterFanControl mtrBaseClusterFanControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterFanControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterFanControl)
initWithDevice_endpointID_queue mtrBaseClusterFanControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterFanControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stepWithParams:completion:@
stepWithParams_completionSelector :: Selector
stepWithParams_completionSelector = mkSelector "stepWithParams:completion:"

-- | @Selector@ for @readAttributeFanModeWithCompletion:@
readAttributeFanModeWithCompletionSelector :: Selector
readAttributeFanModeWithCompletionSelector = mkSelector "readAttributeFanModeWithCompletion:"

-- | @Selector@ for @writeAttributeFanModeWithValue:completion:@
writeAttributeFanModeWithValue_completionSelector :: Selector
writeAttributeFanModeWithValue_completionSelector = mkSelector "writeAttributeFanModeWithValue:completion:"

-- | @Selector@ for @writeAttributeFanModeWithValue:params:completion:@
writeAttributeFanModeWithValue_params_completionSelector :: Selector
writeAttributeFanModeWithValue_params_completionSelector = mkSelector "writeAttributeFanModeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeFanModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFanModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFanModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFanModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeFanModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeFanModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFanModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFanModeSequenceWithCompletion:@
readAttributeFanModeSequenceWithCompletionSelector :: Selector
readAttributeFanModeSequenceWithCompletionSelector = mkSelector "readAttributeFanModeSequenceWithCompletion:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:completion:@
writeAttributeFanModeSequenceWithValue_completionSelector :: Selector
writeAttributeFanModeSequenceWithValue_completionSelector = mkSelector "writeAttributeFanModeSequenceWithValue:completion:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:params:completion:@
writeAttributeFanModeSequenceWithValue_params_completionSelector :: Selector
writeAttributeFanModeSequenceWithValue_params_completionSelector = mkSelector "writeAttributeFanModeSequenceWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeFanModeSequenceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFanModeSequenceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFanModeSequenceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFanModeSequenceWithClusterStateCache:endpoint:queue:completion:@
readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeFanModeSequenceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFanModeSequenceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePercentSettingWithCompletion:@
readAttributePercentSettingWithCompletionSelector :: Selector
readAttributePercentSettingWithCompletionSelector = mkSelector "readAttributePercentSettingWithCompletion:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:completion:@
writeAttributePercentSettingWithValue_completionSelector :: Selector
writeAttributePercentSettingWithValue_completionSelector = mkSelector "writeAttributePercentSettingWithValue:completion:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:params:completion:@
writeAttributePercentSettingWithValue_params_completionSelector :: Selector
writeAttributePercentSettingWithValue_params_completionSelector = mkSelector "writeAttributePercentSettingWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributePercentSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePercentSettingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePercentSettingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePercentSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributePercentSettingWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePercentSettingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePercentSettingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePercentCurrentWithCompletion:@
readAttributePercentCurrentWithCompletionSelector :: Selector
readAttributePercentCurrentWithCompletionSelector = mkSelector "readAttributePercentCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributePercentCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePercentCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePercentCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePercentCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePercentCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePercentCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSpeedMaxWithCompletion:@
readAttributeSpeedMaxWithCompletionSelector :: Selector
readAttributeSpeedMaxWithCompletionSelector = mkSelector "readAttributeSpeedMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributeSpeedMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSpeedMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSpeedMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpeedMaxWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSpeedSettingWithCompletion:@
readAttributeSpeedSettingWithCompletionSelector :: Selector
readAttributeSpeedSettingWithCompletionSelector = mkSelector "readAttributeSpeedSettingWithCompletion:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:completion:@
writeAttributeSpeedSettingWithValue_completionSelector :: Selector
writeAttributeSpeedSettingWithValue_completionSelector = mkSelector "writeAttributeSpeedSettingWithValue:completion:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:params:completion:@
writeAttributeSpeedSettingWithValue_params_completionSelector :: Selector
writeAttributeSpeedSettingWithValue_params_completionSelector = mkSelector "writeAttributeSpeedSettingWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSpeedSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSpeedSettingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedSettingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSpeedSettingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpeedSettingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSpeedCurrentWithCompletion:@
readAttributeSpeedCurrentWithCompletionSelector :: Selector
readAttributeSpeedCurrentWithCompletionSelector = mkSelector "readAttributeSpeedCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeSpeedCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpeedCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRockSupportWithCompletion:@
readAttributeRockSupportWithCompletionSelector :: Selector
readAttributeRockSupportWithCompletionSelector = mkSelector "readAttributeRockSupportWithCompletion:"

-- | @Selector@ for @subscribeAttributeRockSupportWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRockSupportWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRockSupportWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRockSupportWithClusterStateCache:endpoint:queue:completion:@
readAttributeRockSupportWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRockSupportWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRockSupportWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRockSettingWithCompletion:@
readAttributeRockSettingWithCompletionSelector :: Selector
readAttributeRockSettingWithCompletionSelector = mkSelector "readAttributeRockSettingWithCompletion:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:completion:@
writeAttributeRockSettingWithValue_completionSelector :: Selector
writeAttributeRockSettingWithValue_completionSelector = mkSelector "writeAttributeRockSettingWithValue:completion:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:params:completion:@
writeAttributeRockSettingWithValue_params_completionSelector :: Selector
writeAttributeRockSettingWithValue_params_completionSelector = mkSelector "writeAttributeRockSettingWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeRockSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRockSettingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRockSettingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRockSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeRockSettingWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRockSettingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRockSettingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeWindSupportWithCompletion:@
readAttributeWindSupportWithCompletionSelector :: Selector
readAttributeWindSupportWithCompletionSelector = mkSelector "readAttributeWindSupportWithCompletion:"

-- | @Selector@ for @subscribeAttributeWindSupportWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeWindSupportWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindSupportWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindSupportWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindSupportWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeWindSupportWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeWindSupportWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeWindSettingWithCompletion:@
readAttributeWindSettingWithCompletionSelector :: Selector
readAttributeWindSettingWithCompletionSelector = mkSelector "readAttributeWindSettingWithCompletion:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:completion:@
writeAttributeWindSettingWithValue_completionSelector :: Selector
writeAttributeWindSettingWithValue_completionSelector = mkSelector "writeAttributeWindSettingWithValue:completion:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:params:completion:@
writeAttributeWindSettingWithValue_params_completionSelector :: Selector
writeAttributeWindSettingWithValue_params_completionSelector = mkSelector "writeAttributeWindSettingWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeWindSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeWindSettingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindSettingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindSettingWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeWindSettingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeWindSettingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAirflowDirectionWithCompletion:@
readAttributeAirflowDirectionWithCompletionSelector :: Selector
readAttributeAirflowDirectionWithCompletionSelector = mkSelector "readAttributeAirflowDirectionWithCompletion:"

-- | @Selector@ for @writeAttributeAirflowDirectionWithValue:completion:@
writeAttributeAirflowDirectionWithValue_completionSelector :: Selector
writeAttributeAirflowDirectionWithValue_completionSelector = mkSelector "writeAttributeAirflowDirectionWithValue:completion:"

-- | @Selector@ for @writeAttributeAirflowDirectionWithValue:params:completion:@
writeAttributeAirflowDirectionWithValue_params_completionSelector :: Selector
writeAttributeAirflowDirectionWithValue_params_completionSelector = mkSelector "writeAttributeAirflowDirectionWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeAirflowDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAirflowDirectionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAirflowDirectionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAirflowDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAirflowDirectionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAirflowDirectionWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @readAttributeFanModeWithCompletionHandler:@
readAttributeFanModeWithCompletionHandlerSelector :: Selector
readAttributeFanModeWithCompletionHandlerSelector = mkSelector "readAttributeFanModeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeFanModeWithValue:completionHandler:@
writeAttributeFanModeWithValue_completionHandlerSelector :: Selector
writeAttributeFanModeWithValue_completionHandlerSelector = mkSelector "writeAttributeFanModeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeFanModeWithValue:params:completionHandler:@
writeAttributeFanModeWithValue_params_completionHandlerSelector :: Selector
writeAttributeFanModeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeFanModeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeFanModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFanModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFanModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFanModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeFanModeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFanModeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFanModeSequenceWithCompletionHandler:@
readAttributeFanModeSequenceWithCompletionHandlerSelector :: Selector
readAttributeFanModeSequenceWithCompletionHandlerSelector = mkSelector "readAttributeFanModeSequenceWithCompletionHandler:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:completionHandler:@
writeAttributeFanModeSequenceWithValue_completionHandlerSelector :: Selector
writeAttributeFanModeSequenceWithValue_completionHandlerSelector = mkSelector "writeAttributeFanModeSequenceWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:params:completionHandler:@
writeAttributeFanModeSequenceWithValue_params_completionHandlerSelector :: Selector
writeAttributeFanModeSequenceWithValue_params_completionHandlerSelector = mkSelector "writeAttributeFanModeSequenceWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeFanModeSequenceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFanModeSequenceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFanModeSequenceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFanModeSequenceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeFanModeSequenceWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFanModeSequenceWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePercentSettingWithCompletionHandler:@
readAttributePercentSettingWithCompletionHandlerSelector :: Selector
readAttributePercentSettingWithCompletionHandlerSelector = mkSelector "readAttributePercentSettingWithCompletionHandler:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:completionHandler:@
writeAttributePercentSettingWithValue_completionHandlerSelector :: Selector
writeAttributePercentSettingWithValue_completionHandlerSelector = mkSelector "writeAttributePercentSettingWithValue:completionHandler:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:params:completionHandler:@
writeAttributePercentSettingWithValue_params_completionHandlerSelector :: Selector
writeAttributePercentSettingWithValue_params_completionHandlerSelector = mkSelector "writeAttributePercentSettingWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributePercentSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePercentSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePercentSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePercentSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePercentSettingWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePercentSettingWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePercentCurrentWithCompletionHandler:@
readAttributePercentCurrentWithCompletionHandlerSelector :: Selector
readAttributePercentCurrentWithCompletionHandlerSelector = mkSelector "readAttributePercentCurrentWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePercentCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePercentCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePercentCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePercentCurrentWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePercentCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePercentCurrentWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSpeedMaxWithCompletionHandler:@
readAttributeSpeedMaxWithCompletionHandlerSelector :: Selector
readAttributeSpeedMaxWithCompletionHandlerSelector = mkSelector "readAttributeSpeedMaxWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSpeedMaxWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSpeedMaxWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedMaxWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedMaxWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSpeedMaxWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSpeedMaxWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSpeedSettingWithCompletionHandler:@
readAttributeSpeedSettingWithCompletionHandlerSelector :: Selector
readAttributeSpeedSettingWithCompletionHandlerSelector = mkSelector "readAttributeSpeedSettingWithCompletionHandler:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:completionHandler:@
writeAttributeSpeedSettingWithValue_completionHandlerSelector :: Selector
writeAttributeSpeedSettingWithValue_completionHandlerSelector = mkSelector "writeAttributeSpeedSettingWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:params:completionHandler:@
writeAttributeSpeedSettingWithValue_params_completionHandlerSelector :: Selector
writeAttributeSpeedSettingWithValue_params_completionHandlerSelector = mkSelector "writeAttributeSpeedSettingWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeSpeedSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSpeedSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSpeedSettingWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSpeedSettingWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSpeedCurrentWithCompletionHandler:@
readAttributeSpeedCurrentWithCompletionHandlerSelector :: Selector
readAttributeSpeedCurrentWithCompletionHandlerSelector = mkSelector "readAttributeSpeedCurrentWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSpeedCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSpeedCurrentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpeedCurrentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpeedCurrentWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSpeedCurrentWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSpeedCurrentWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeRockSupportWithCompletionHandler:@
readAttributeRockSupportWithCompletionHandlerSelector :: Selector
readAttributeRockSupportWithCompletionHandlerSelector = mkSelector "readAttributeRockSupportWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeRockSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRockSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRockSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRockSupportWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeRockSupportWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeRockSupportWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeRockSettingWithCompletionHandler:@
readAttributeRockSettingWithCompletionHandlerSelector :: Selector
readAttributeRockSettingWithCompletionHandlerSelector = mkSelector "readAttributeRockSettingWithCompletionHandler:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:completionHandler:@
writeAttributeRockSettingWithValue_completionHandlerSelector :: Selector
writeAttributeRockSettingWithValue_completionHandlerSelector = mkSelector "writeAttributeRockSettingWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:params:completionHandler:@
writeAttributeRockSettingWithValue_params_completionHandlerSelector :: Selector
writeAttributeRockSettingWithValue_params_completionHandlerSelector = mkSelector "writeAttributeRockSettingWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeRockSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRockSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRockSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRockSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeRockSettingWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeRockSettingWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeWindSupportWithCompletionHandler:@
readAttributeWindSupportWithCompletionHandlerSelector :: Selector
readAttributeWindSupportWithCompletionHandlerSelector = mkSelector "readAttributeWindSupportWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeWindSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeWindSupportWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindSupportWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindSupportWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeWindSupportWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeWindSupportWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeWindSettingWithCompletionHandler:@
readAttributeWindSettingWithCompletionHandlerSelector :: Selector
readAttributeWindSettingWithCompletionHandlerSelector = mkSelector "readAttributeWindSettingWithCompletionHandler:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:completionHandler:@
writeAttributeWindSettingWithValue_completionHandlerSelector :: Selector
writeAttributeWindSettingWithValue_completionHandlerSelector = mkSelector "writeAttributeWindSettingWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:params:completionHandler:@
writeAttributeWindSettingWithValue_params_completionHandlerSelector :: Selector
writeAttributeWindSettingWithValue_params_completionHandlerSelector = mkSelector "writeAttributeWindSettingWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeWindSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeWindSettingWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindSettingWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindSettingWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeWindSettingWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeWindSettingWithAttributeCache:endpoint:queue:completionHandler:"

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

