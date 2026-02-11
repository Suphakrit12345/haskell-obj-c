{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Level Control
--
-- Attributes and commands for controlling devices that can be set to a level between fully 'On' and fully 'Off.'
--
-- Generated bindings for @MTRBaseClusterLevelControl@.
module ObjC.Matter.MTRBaseClusterLevelControl
  ( MTRBaseClusterLevelControl
  , IsMTRBaseClusterLevelControl(..)
  , moveToLevelWithParams_completion
  , moveWithParams_completion
  , stepWithParams_completion
  , stopWithParams_completion
  , moveToLevelWithOnOffWithParams_completion
  , moveWithOnOffWithParams_completion
  , stepWithOnOffWithParams_completion
  , stopWithOnOffWithParams_completion
  , moveToClosestFrequencyWithParams_completion
  , readAttributeCurrentLevelWithCompletion
  , subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeRemainingTimeWithCompletion
  , subscribeAttributeRemainingTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeRemainingTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinLevelWithCompletion
  , subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxLevelWithCompletion
  , subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentFrequencyWithCompletion
  , subscribeAttributeCurrentFrequencyWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentFrequencyWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinFrequencyWithCompletion
  , subscribeAttributeMinFrequencyWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinFrequencyWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxFrequencyWithCompletion
  , subscribeAttributeMaxFrequencyWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxFrequencyWithClusterStateCache_endpoint_queue_completion
  , readAttributeOptionsWithCompletion
  , writeAttributeOptionsWithValue_completion
  , writeAttributeOptionsWithValue_params_completion
  , subscribeAttributeOptionsWithParams_subscriptionEstablished_reportHandler
  , readAttributeOptionsWithClusterStateCache_endpoint_queue_completion
  , readAttributeOnOffTransitionTimeWithCompletion
  , writeAttributeOnOffTransitionTimeWithValue_completion
  , writeAttributeOnOffTransitionTimeWithValue_params_completion
  , subscribeAttributeOnOffTransitionTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnOffTransitionTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeOnLevelWithCompletion
  , writeAttributeOnLevelWithValue_completion
  , writeAttributeOnLevelWithValue_params_completion
  , subscribeAttributeOnLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeOnTransitionTimeWithCompletion
  , writeAttributeOnTransitionTimeWithValue_completion
  , writeAttributeOnTransitionTimeWithValue_params_completion
  , subscribeAttributeOnTransitionTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnTransitionTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeOffTransitionTimeWithCompletion
  , writeAttributeOffTransitionTimeWithValue_completion
  , writeAttributeOffTransitionTimeWithValue_params_completion
  , subscribeAttributeOffTransitionTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeOffTransitionTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultMoveRateWithCompletion
  , writeAttributeDefaultMoveRateWithValue_completion
  , writeAttributeDefaultMoveRateWithValue_params_completion
  , subscribeAttributeDefaultMoveRateWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultMoveRateWithClusterStateCache_endpoint_queue_completion
  , readAttributeStartUpCurrentLevelWithCompletion
  , writeAttributeStartUpCurrentLevelWithValue_completion
  , writeAttributeStartUpCurrentLevelWithValue_params_completion
  , subscribeAttributeStartUpCurrentLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeStartUpCurrentLevelWithClusterStateCache_endpoint_queue_completion
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
  , moveToLevelWithParams_completionHandler
  , moveWithParams_completionHandler
  , stepWithParams_completionHandler
  , stopWithParams_completionHandler
  , moveToLevelWithOnOffWithParams_completionHandler
  , moveWithOnOffWithParams_completionHandler
  , stepWithOnOffWithParams_completionHandler
  , stopWithOnOffWithParams_completionHandler
  , moveToClosestFrequencyWithParams_completionHandler
  , readAttributeCurrentLevelWithCompletionHandler
  , subscribeAttributeCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentLevelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeRemainingTimeWithCompletionHandler
  , subscribeAttributeRemainingTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeRemainingTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMinLevelWithCompletionHandler
  , subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMaxLevelWithCompletionHandler
  , subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentFrequencyWithCompletionHandler
  , subscribeAttributeCurrentFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentFrequencyWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMinFrequencyWithCompletionHandler
  , subscribeAttributeMinFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMinFrequencyWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMaxFrequencyWithCompletionHandler
  , subscribeAttributeMaxFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMaxFrequencyWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOptionsWithCompletionHandler
  , writeAttributeOptionsWithValue_completionHandler
  , writeAttributeOptionsWithValue_params_completionHandler
  , subscribeAttributeOptionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOptionsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOnOffTransitionTimeWithCompletionHandler
  , writeAttributeOnOffTransitionTimeWithValue_completionHandler
  , writeAttributeOnOffTransitionTimeWithValue_params_completionHandler
  , subscribeAttributeOnOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOnOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOnLevelWithCompletionHandler
  , writeAttributeOnLevelWithValue_completionHandler
  , writeAttributeOnLevelWithValue_params_completionHandler
  , subscribeAttributeOnLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOnLevelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOnTransitionTimeWithCompletionHandler
  , writeAttributeOnTransitionTimeWithValue_completionHandler
  , writeAttributeOnTransitionTimeWithValue_params_completionHandler
  , subscribeAttributeOnTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOnTransitionTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOffTransitionTimeWithCompletionHandler
  , writeAttributeOffTransitionTimeWithValue_completionHandler
  , writeAttributeOffTransitionTimeWithValue_params_completionHandler
  , subscribeAttributeOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeDefaultMoveRateWithCompletionHandler
  , writeAttributeDefaultMoveRateWithValue_completionHandler
  , writeAttributeDefaultMoveRateWithValue_params_completionHandler
  , subscribeAttributeDefaultMoveRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeDefaultMoveRateWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStartUpCurrentLevelWithCompletionHandler
  , writeAttributeStartUpCurrentLevelWithValue_completionHandler
  , writeAttributeStartUpCurrentLevelWithValue_params_completionHandler
  , subscribeAttributeStartUpCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStartUpCurrentLevelWithAttributeCache_endpoint_queue_completionHandler
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
  , moveToLevelWithParams_completionSelector
  , moveWithParams_completionSelector
  , stepWithParams_completionSelector
  , stopWithParams_completionSelector
  , moveToLevelWithOnOffWithParams_completionSelector
  , moveWithOnOffWithParams_completionSelector
  , stepWithOnOffWithParams_completionSelector
  , stopWithOnOffWithParams_completionSelector
  , moveToClosestFrequencyWithParams_completionSelector
  , readAttributeCurrentLevelWithCompletionSelector
  , subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRemainingTimeWithCompletionSelector
  , subscribeAttributeRemainingTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRemainingTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinLevelWithCompletionSelector
  , subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxLevelWithCompletionSelector
  , subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentFrequencyWithCompletionSelector
  , subscribeAttributeCurrentFrequencyWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentFrequencyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinFrequencyWithCompletionSelector
  , subscribeAttributeMinFrequencyWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinFrequencyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxFrequencyWithCompletionSelector
  , subscribeAttributeMaxFrequencyWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxFrequencyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOptionsWithCompletionSelector
  , writeAttributeOptionsWithValue_completionSelector
  , writeAttributeOptionsWithValue_params_completionSelector
  , subscribeAttributeOptionsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOptionsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnOffTransitionTimeWithCompletionSelector
  , writeAttributeOnOffTransitionTimeWithValue_completionSelector
  , writeAttributeOnOffTransitionTimeWithValue_params_completionSelector
  , subscribeAttributeOnOffTransitionTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnOffTransitionTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnLevelWithCompletionSelector
  , writeAttributeOnLevelWithValue_completionSelector
  , writeAttributeOnLevelWithValue_params_completionSelector
  , subscribeAttributeOnLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnTransitionTimeWithCompletionSelector
  , writeAttributeOnTransitionTimeWithValue_completionSelector
  , writeAttributeOnTransitionTimeWithValue_params_completionSelector
  , subscribeAttributeOnTransitionTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnTransitionTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOffTransitionTimeWithCompletionSelector
  , writeAttributeOffTransitionTimeWithValue_completionSelector
  , writeAttributeOffTransitionTimeWithValue_params_completionSelector
  , subscribeAttributeOffTransitionTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOffTransitionTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultMoveRateWithCompletionSelector
  , writeAttributeDefaultMoveRateWithValue_completionSelector
  , writeAttributeDefaultMoveRateWithValue_params_completionSelector
  , subscribeAttributeDefaultMoveRateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDefaultMoveRateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStartUpCurrentLevelWithCompletionSelector
  , writeAttributeStartUpCurrentLevelWithValue_completionSelector
  , writeAttributeStartUpCurrentLevelWithValue_params_completionSelector
  , subscribeAttributeStartUpCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStartUpCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector
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
  , moveToLevelWithParams_completionHandlerSelector
  , moveWithParams_completionHandlerSelector
  , stepWithParams_completionHandlerSelector
  , stopWithParams_completionHandlerSelector
  , moveToLevelWithOnOffWithParams_completionHandlerSelector
  , moveWithOnOffWithParams_completionHandlerSelector
  , stepWithOnOffWithParams_completionHandlerSelector
  , stopWithOnOffWithParams_completionHandlerSelector
  , moveToClosestFrequencyWithParams_completionHandlerSelector
  , readAttributeCurrentLevelWithCompletionHandlerSelector
  , subscribeAttributeCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentLevelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeRemainingTimeWithCompletionHandlerSelector
  , subscribeAttributeRemainingTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeRemainingTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMinLevelWithCompletionHandlerSelector
  , subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMaxLevelWithCompletionHandlerSelector
  , subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentFrequencyWithCompletionHandlerSelector
  , subscribeAttributeCurrentFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentFrequencyWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMinFrequencyWithCompletionHandlerSelector
  , subscribeAttributeMinFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinFrequencyWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMaxFrequencyWithCompletionHandlerSelector
  , subscribeAttributeMaxFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxFrequencyWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOptionsWithCompletionHandlerSelector
  , writeAttributeOptionsWithValue_completionHandlerSelector
  , writeAttributeOptionsWithValue_params_completionHandlerSelector
  , subscribeAttributeOptionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOptionsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOnOffTransitionTimeWithCompletionHandlerSelector
  , writeAttributeOnOffTransitionTimeWithValue_completionHandlerSelector
  , writeAttributeOnOffTransitionTimeWithValue_params_completionHandlerSelector
  , subscribeAttributeOnOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOnLevelWithCompletionHandlerSelector
  , writeAttributeOnLevelWithValue_completionHandlerSelector
  , writeAttributeOnLevelWithValue_params_completionHandlerSelector
  , subscribeAttributeOnLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnLevelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOnTransitionTimeWithCompletionHandlerSelector
  , writeAttributeOnTransitionTimeWithValue_completionHandlerSelector
  , writeAttributeOnTransitionTimeWithValue_params_completionHandlerSelector
  , subscribeAttributeOnTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnTransitionTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOffTransitionTimeWithCompletionHandlerSelector
  , writeAttributeOffTransitionTimeWithValue_completionHandlerSelector
  , writeAttributeOffTransitionTimeWithValue_params_completionHandlerSelector
  , subscribeAttributeOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeDefaultMoveRateWithCompletionHandlerSelector
  , writeAttributeDefaultMoveRateWithValue_completionHandlerSelector
  , writeAttributeDefaultMoveRateWithValue_params_completionHandlerSelector
  , subscribeAttributeDefaultMoveRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeDefaultMoveRateWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStartUpCurrentLevelWithCompletionHandlerSelector
  , writeAttributeStartUpCurrentLevelWithValue_completionHandlerSelector
  , writeAttributeStartUpCurrentLevelWithValue_params_completionHandlerSelector
  , subscribeAttributeStartUpCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeStartUpCurrentLevelWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command MoveToLevel
--
-- This command will move the device to the specified level.
--
-- ObjC selector: @- moveToLevelWithParams:completion:@
moveToLevelWithParams_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterMoveToLevelParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
moveToLevelWithParams_completion mtrBaseClusterLevelControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "moveToLevelWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command Move
--
-- This command will move the device using the specified values.
--
-- ObjC selector: @- moveWithParams:completion:@
moveWithParams_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterMoveParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
moveWithParams_completion mtrBaseClusterLevelControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "moveWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command Step
--
-- This command will do a relative step change of the device using the specified values.
--
-- ObjC selector: @- stepWithParams:completion:@
stepWithParams_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterStepParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
stepWithParams_completion mtrBaseClusterLevelControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "stepWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command Stop
--
-- This command will stop the actions of various other commands that are still in progress.
--
-- ObjC selector: @- stopWithParams:completion:@
stopWithParams_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterStopParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
stopWithParams_completion mtrBaseClusterLevelControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "stopWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command MoveToLevelWithOnOff
--
-- Command description for MoveToLevelWithOnOff
--
-- ObjC selector: @- moveToLevelWithOnOffWithParams:completion:@
moveToLevelWithOnOffWithParams_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterMoveToLevelWithOnOffParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
moveToLevelWithOnOffWithParams_completion mtrBaseClusterLevelControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "moveToLevelWithOnOffWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command MoveWithOnOff
--
-- Command description for MoveWithOnOff
--
-- ObjC selector: @- moveWithOnOffWithParams:completion:@
moveWithOnOffWithParams_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterMoveWithOnOffParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
moveWithOnOffWithParams_completion mtrBaseClusterLevelControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "moveWithOnOffWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command StepWithOnOff
--
-- Command description for StepWithOnOff
--
-- ObjC selector: @- stepWithOnOffWithParams:completion:@
stepWithOnOffWithParams_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterStepWithOnOffParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
stepWithOnOffWithParams_completion mtrBaseClusterLevelControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "stepWithOnOffWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command StopWithOnOff
--
-- Command description for StopWithOnOff
--
-- ObjC selector: @- stopWithOnOffWithParams:completion:@
stopWithOnOffWithParams_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterStopWithOnOffParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
stopWithOnOffWithParams_completion mtrBaseClusterLevelControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "stopWithOnOffWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command MoveToClosestFrequency
--
-- This command will cause the device to change the current frequency to the requested value.
--
-- ObjC selector: @- moveToClosestFrequencyWithParams:completion:@
moveToClosestFrequencyWithParams_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterMoveToClosestFrequencyParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
moveToClosestFrequencyWithParams_completion mtrBaseClusterLevelControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "moveToClosestFrequencyWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentLevelWithCompletion:@
readAttributeCurrentLevelWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeCurrentLevelWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeCurrentLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeCurrentLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRemainingTimeWithCompletion:@
readAttributeRemainingTimeWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeRemainingTimeWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeRemainingTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRemainingTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRemainingTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeRemainingTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRemainingTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeRemainingTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRemainingTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRemainingTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMinLevelWithCompletion:@
readAttributeMinLevelWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeMinLevelWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeMinLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMinLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeMinLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxLevelWithCompletion:@
readAttributeMaxLevelWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeMaxLevelWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeMaxLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeMaxLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentFrequencyWithCompletion:@
readAttributeCurrentFrequencyWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeCurrentFrequencyWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeCurrentFrequencyWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentFrequencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFrequencyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentFrequencyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeCurrentFrequencyWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentFrequencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentFrequencyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentFrequencyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentFrequencyWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMinFrequencyWithCompletion:@
readAttributeMinFrequencyWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeMinFrequencyWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeMinFrequencyWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMinFrequencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinFrequencyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinFrequencyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeMinFrequencyWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinFrequencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinFrequencyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinFrequencyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinFrequencyWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxFrequencyWithCompletion:@
readAttributeMaxFrequencyWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeMaxFrequencyWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeMaxFrequencyWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxFrequencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxFrequencyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxFrequencyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeMaxFrequencyWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxFrequencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxFrequencyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxFrequencyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxFrequencyWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOptionsWithCompletion:@
readAttributeOptionsWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeOptionsWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeOptionsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOptionsWithValue:completion:@
writeAttributeOptionsWithValue_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeOptionsWithValue_completion mtrBaseClusterLevelControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOptionsWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOptionsWithValue:params:completion:@
writeAttributeOptionsWithValue_params_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeOptionsWithValue_params_completion mtrBaseClusterLevelControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOptionsWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOptionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOptionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOptionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeOptionsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOptionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeOptionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOptionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOptionsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOnOffTransitionTimeWithCompletion:@
readAttributeOnOffTransitionTimeWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeOnOffTransitionTimeWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeOnOffTransitionTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOnOffTransitionTimeWithValue:completion:@
writeAttributeOnOffTransitionTimeWithValue_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeOnOffTransitionTimeWithValue_completion mtrBaseClusterLevelControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnOffTransitionTimeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOnOffTransitionTimeWithValue:params:completion:@
writeAttributeOnOffTransitionTimeWithValue_params_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeOnOffTransitionTimeWithValue_params_completion mtrBaseClusterLevelControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnOffTransitionTimeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOnOffTransitionTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffTransitionTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnOffTransitionTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeOnOffTransitionTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnOffTransitionTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnOffTransitionTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnOffTransitionTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnOffTransitionTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOnLevelWithCompletion:@
readAttributeOnLevelWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeOnLevelWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeOnLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOnLevelWithValue:completion:@
writeAttributeOnLevelWithValue_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeOnLevelWithValue_completion mtrBaseClusterLevelControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnLevelWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOnLevelWithValue:params:completion:@
writeAttributeOnLevelWithValue_params_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeOnLevelWithValue_params_completion mtrBaseClusterLevelControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnLevelWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOnLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeOnLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOnTransitionTimeWithCompletion:@
readAttributeOnTransitionTimeWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeOnTransitionTimeWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeOnTransitionTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOnTransitionTimeWithValue:completion:@
writeAttributeOnTransitionTimeWithValue_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeOnTransitionTimeWithValue_completion mtrBaseClusterLevelControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnTransitionTimeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOnTransitionTimeWithValue:params:completion:@
writeAttributeOnTransitionTimeWithValue_params_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeOnTransitionTimeWithValue_params_completion mtrBaseClusterLevelControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnTransitionTimeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOnTransitionTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTransitionTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnTransitionTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeOnTransitionTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnTransitionTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnTransitionTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnTransitionTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnTransitionTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOffTransitionTimeWithCompletion:@
readAttributeOffTransitionTimeWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeOffTransitionTimeWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeOffTransitionTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOffTransitionTimeWithValue:completion:@
writeAttributeOffTransitionTimeWithValue_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeOffTransitionTimeWithValue_completion mtrBaseClusterLevelControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOffTransitionTimeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOffTransitionTimeWithValue:params:completion:@
writeAttributeOffTransitionTimeWithValue_params_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeOffTransitionTimeWithValue_params_completion mtrBaseClusterLevelControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOffTransitionTimeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOffTransitionTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOffTransitionTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOffTransitionTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeOffTransitionTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOffTransitionTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOffTransitionTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOffTransitionTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOffTransitionTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDefaultMoveRateWithCompletion:@
readAttributeDefaultMoveRateWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeDefaultMoveRateWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeDefaultMoveRateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeDefaultMoveRateWithValue:completion:@
writeAttributeDefaultMoveRateWithValue_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeDefaultMoveRateWithValue_completion mtrBaseClusterLevelControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeDefaultMoveRateWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeDefaultMoveRateWithValue:params:completion:@
writeAttributeDefaultMoveRateWithValue_params_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeDefaultMoveRateWithValue_params_completion mtrBaseClusterLevelControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeDefaultMoveRateWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDefaultMoveRateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultMoveRateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultMoveRateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeDefaultMoveRateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDefaultMoveRateWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultMoveRateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultMoveRateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDefaultMoveRateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStartUpCurrentLevelWithCompletion:@
readAttributeStartUpCurrentLevelWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeStartUpCurrentLevelWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeStartUpCurrentLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeStartUpCurrentLevelWithValue:completion:@
writeAttributeStartUpCurrentLevelWithValue_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeStartUpCurrentLevelWithValue_completion mtrBaseClusterLevelControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeStartUpCurrentLevelWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeStartUpCurrentLevelWithValue:params:completion:@
writeAttributeStartUpCurrentLevelWithValue_params_completion :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeStartUpCurrentLevelWithValue_params_completion mtrBaseClusterLevelControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeStartUpCurrentLevelWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStartUpCurrentLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpCurrentLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartUpCurrentLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeStartUpCurrentLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStartUpCurrentLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartUpCurrentLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartUpCurrentLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStartUpCurrentLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterLevelControl  completion =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> IO (Id MTRBaseClusterLevelControl)
init_ mtrBaseClusterLevelControl  =
    sendMsg mtrBaseClusterLevelControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterLevelControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterLevelControl -> device -> CUShort -> queue -> IO (Id MTRBaseClusterLevelControl)
initWithDevice_endpoint_queue mtrBaseClusterLevelControl  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- moveToLevelWithParams:completionHandler:@
moveToLevelWithParams_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterMoveToLevelParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
moveToLevelWithParams_completionHandler mtrBaseClusterLevelControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "moveToLevelWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveWithParams:completionHandler:@
moveWithParams_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterMoveParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
moveWithParams_completionHandler mtrBaseClusterLevelControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "moveWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stepWithParams:completionHandler:@
stepWithParams_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterStepParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
stepWithParams_completionHandler mtrBaseClusterLevelControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "stepWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopWithParams:completionHandler:@
stopWithParams_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterStopParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
stopWithParams_completionHandler mtrBaseClusterLevelControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "stopWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveToLevelWithOnOffWithParams:completionHandler:@
moveToLevelWithOnOffWithParams_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterMoveToLevelWithOnOffParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
moveToLevelWithOnOffWithParams_completionHandler mtrBaseClusterLevelControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "moveToLevelWithOnOffWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveWithOnOffWithParams:completionHandler:@
moveWithOnOffWithParams_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterMoveWithOnOffParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
moveWithOnOffWithParams_completionHandler mtrBaseClusterLevelControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "moveWithOnOffWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stepWithOnOffWithParams:completionHandler:@
stepWithOnOffWithParams_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterStepWithOnOffParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
stepWithOnOffWithParams_completionHandler mtrBaseClusterLevelControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "stepWithOnOffWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopWithOnOffWithParams:completionHandler:@
stopWithOnOffWithParams_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterStopWithOnOffParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
stopWithOnOffWithParams_completionHandler mtrBaseClusterLevelControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "stopWithOnOffWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveToClosestFrequencyWithParams:completionHandler:@
moveToClosestFrequencyWithParams_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRLevelControlClusterMoveToClosestFrequencyParams params) => mtrBaseClusterLevelControl -> params -> Ptr () -> IO ()
moveToClosestFrequencyWithParams_completionHandler mtrBaseClusterLevelControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "moveToClosestFrequencyWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCurrentLevelWithCompletionHandler:@
readAttributeCurrentLevelWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeCurrentLevelWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeCurrentLevelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCurrentLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeCurrentLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentLevelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentLevelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentLevelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeRemainingTimeWithCompletionHandler:@
readAttributeRemainingTimeWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeRemainingTimeWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeRemainingTimeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeRemainingTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRemainingTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeRemainingTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRemainingTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRemainingTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRemainingTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRemainingTimeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMinLevelWithCompletionHandler:@
readAttributeMinLevelWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeMinLevelWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeMinLevelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinLevelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMaxLevelWithCompletionHandler:@
readAttributeMaxLevelWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeMaxLevelWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeMaxLevelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxLevelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCurrentFrequencyWithCompletionHandler:@
readAttributeCurrentFrequencyWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeCurrentFrequencyWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeCurrentFrequencyWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCurrentFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeCurrentFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentFrequencyWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentFrequencyWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentFrequencyWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentFrequencyWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMinFrequencyWithCompletionHandler:@
readAttributeMinFrequencyWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeMinFrequencyWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeMinFrequencyWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMinFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMinFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeMinFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinFrequencyWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMinFrequencyWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinFrequencyWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinFrequencyWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMaxFrequencyWithCompletionHandler:@
readAttributeMaxFrequencyWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeMaxFrequencyWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeMaxFrequencyWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMaxFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeMaxFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxFrequencyWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxFrequencyWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxFrequencyWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxFrequencyWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOptionsWithCompletionHandler:@
readAttributeOptionsWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeOptionsWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeOptionsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOptionsWithValue:completionHandler:@
writeAttributeOptionsWithValue_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeOptionsWithValue_completionHandler mtrBaseClusterLevelControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOptionsWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOptionsWithValue:params:completionHandler:@
writeAttributeOptionsWithValue_params_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeOptionsWithValue_params_completionHandler mtrBaseClusterLevelControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOptionsWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOptionsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOptionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOptionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeOptionsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOptionsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOptionsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOptionsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOptionsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOnOffTransitionTimeWithCompletionHandler:@
readAttributeOnOffTransitionTimeWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeOnOffTransitionTimeWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeOnOffTransitionTimeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOnOffTransitionTimeWithValue:completionHandler:@
writeAttributeOnOffTransitionTimeWithValue_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeOnOffTransitionTimeWithValue_completionHandler mtrBaseClusterLevelControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnOffTransitionTimeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOnOffTransitionTimeWithValue:params:completionHandler:@
writeAttributeOnOffTransitionTimeWithValue_params_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeOnOffTransitionTimeWithValue_params_completionHandler mtrBaseClusterLevelControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnOffTransitionTimeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOnOffTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeOnOffTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnOffTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnOffTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOnLevelWithCompletionHandler:@
readAttributeOnLevelWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeOnLevelWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeOnLevelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOnLevelWithValue:completionHandler:@
writeAttributeOnLevelWithValue_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeOnLevelWithValue_completionHandler mtrBaseClusterLevelControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnLevelWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOnLevelWithValue:params:completionHandler:@
writeAttributeOnLevelWithValue_params_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeOnLevelWithValue_params_completionHandler mtrBaseClusterLevelControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnLevelWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOnLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeOnLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnLevelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnLevelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnLevelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOnTransitionTimeWithCompletionHandler:@
readAttributeOnTransitionTimeWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeOnTransitionTimeWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeOnTransitionTimeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOnTransitionTimeWithValue:completionHandler:@
writeAttributeOnTransitionTimeWithValue_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeOnTransitionTimeWithValue_completionHandler mtrBaseClusterLevelControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnTransitionTimeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOnTransitionTimeWithValue:params:completionHandler:@
writeAttributeOnTransitionTimeWithValue_params_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeOnTransitionTimeWithValue_params_completionHandler mtrBaseClusterLevelControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOnTransitionTimeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOnTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeOnTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnTransitionTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnTransitionTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOffTransitionTimeWithCompletionHandler:@
readAttributeOffTransitionTimeWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeOffTransitionTimeWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeOffTransitionTimeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOffTransitionTimeWithValue:completionHandler:@
writeAttributeOffTransitionTimeWithValue_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeOffTransitionTimeWithValue_completionHandler mtrBaseClusterLevelControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOffTransitionTimeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOffTransitionTimeWithValue:params:completionHandler:@
writeAttributeOffTransitionTimeWithValue_params_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeOffTransitionTimeWithValue_params_completionHandler mtrBaseClusterLevelControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeOffTransitionTimeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOffTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeOffTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOffTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOffTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeDefaultMoveRateWithCompletionHandler:@
readAttributeDefaultMoveRateWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeDefaultMoveRateWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeDefaultMoveRateWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeDefaultMoveRateWithValue:completionHandler:@
writeAttributeDefaultMoveRateWithValue_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeDefaultMoveRateWithValue_completionHandler mtrBaseClusterLevelControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeDefaultMoveRateWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeDefaultMoveRateWithValue:params:completionHandler:@
writeAttributeDefaultMoveRateWithValue_params_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeDefaultMoveRateWithValue_params_completionHandler mtrBaseClusterLevelControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeDefaultMoveRateWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeDefaultMoveRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultMoveRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultMoveRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeDefaultMoveRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDefaultMoveRateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDefaultMoveRateWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultMoveRateWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDefaultMoveRateWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeStartUpCurrentLevelWithCompletionHandler:@
readAttributeStartUpCurrentLevelWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeStartUpCurrentLevelWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeStartUpCurrentLevelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeStartUpCurrentLevelWithValue:completionHandler:@
writeAttributeStartUpCurrentLevelWithValue_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value) => mtrBaseClusterLevelControl -> value -> Ptr () -> IO ()
writeAttributeStartUpCurrentLevelWithValue_completionHandler mtrBaseClusterLevelControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeStartUpCurrentLevelWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeStartUpCurrentLevelWithValue:params:completionHandler:@
writeAttributeStartUpCurrentLevelWithValue_params_completionHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLevelControl -> value -> params -> Ptr () -> IO ()
writeAttributeStartUpCurrentLevelWithValue_params_completionHandler mtrBaseClusterLevelControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLevelControl (mkSelector "writeAttributeStartUpCurrentLevelWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeStartUpCurrentLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartUpCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeStartUpCurrentLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStartUpCurrentLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartUpCurrentLevelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartUpCurrentLevelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStartUpCurrentLevelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl => mtrBaseClusterLevelControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterLevelControl  completionHandler =
    sendMsg mtrBaseClusterLevelControl (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterLevelControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterLevelControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterLevelControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterLevelControl mtrBaseClusterLevelControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterLevelControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterLevelControl)
initWithDevice_endpointID_queue mtrBaseClusterLevelControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterLevelControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @moveToLevelWithParams:completion:@
moveToLevelWithParams_completionSelector :: Selector
moveToLevelWithParams_completionSelector = mkSelector "moveToLevelWithParams:completion:"

-- | @Selector@ for @moveWithParams:completion:@
moveWithParams_completionSelector :: Selector
moveWithParams_completionSelector = mkSelector "moveWithParams:completion:"

-- | @Selector@ for @stepWithParams:completion:@
stepWithParams_completionSelector :: Selector
stepWithParams_completionSelector = mkSelector "stepWithParams:completion:"

-- | @Selector@ for @stopWithParams:completion:@
stopWithParams_completionSelector :: Selector
stopWithParams_completionSelector = mkSelector "stopWithParams:completion:"

-- | @Selector@ for @moveToLevelWithOnOffWithParams:completion:@
moveToLevelWithOnOffWithParams_completionSelector :: Selector
moveToLevelWithOnOffWithParams_completionSelector = mkSelector "moveToLevelWithOnOffWithParams:completion:"

-- | @Selector@ for @moveWithOnOffWithParams:completion:@
moveWithOnOffWithParams_completionSelector :: Selector
moveWithOnOffWithParams_completionSelector = mkSelector "moveWithOnOffWithParams:completion:"

-- | @Selector@ for @stepWithOnOffWithParams:completion:@
stepWithOnOffWithParams_completionSelector :: Selector
stepWithOnOffWithParams_completionSelector = mkSelector "stepWithOnOffWithParams:completion:"

-- | @Selector@ for @stopWithOnOffWithParams:completion:@
stopWithOnOffWithParams_completionSelector :: Selector
stopWithOnOffWithParams_completionSelector = mkSelector "stopWithOnOffWithParams:completion:"

-- | @Selector@ for @moveToClosestFrequencyWithParams:completion:@
moveToClosestFrequencyWithParams_completionSelector :: Selector
moveToClosestFrequencyWithParams_completionSelector = mkSelector "moveToClosestFrequencyWithParams:completion:"

-- | @Selector@ for @readAttributeCurrentLevelWithCompletion:@
readAttributeCurrentLevelWithCompletionSelector :: Selector
readAttributeCurrentLevelWithCompletionSelector = mkSelector "readAttributeCurrentLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRemainingTimeWithCompletion:@
readAttributeRemainingTimeWithCompletionSelector :: Selector
readAttributeRemainingTimeWithCompletionSelector = mkSelector "readAttributeRemainingTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeRemainingTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRemainingTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRemainingTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRemainingTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeRemainingTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRemainingTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRemainingTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinLevelWithCompletion:@
readAttributeMinLevelWithCompletionSelector :: Selector
readAttributeMinLevelWithCompletionSelector = mkSelector "readAttributeMinLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributeMinLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMinLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxLevelWithCompletion:@
readAttributeMaxLevelWithCompletionSelector :: Selector
readAttributeMaxLevelWithCompletionSelector = mkSelector "readAttributeMaxLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentFrequencyWithCompletion:@
readAttributeCurrentFrequencyWithCompletionSelector :: Selector
readAttributeCurrentFrequencyWithCompletionSelector = mkSelector "readAttributeCurrentFrequencyWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentFrequencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFrequencyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentFrequencyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentFrequencyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentFrequencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentFrequencyWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentFrequencyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentFrequencyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinFrequencyWithCompletion:@
readAttributeMinFrequencyWithCompletionSelector :: Selector
readAttributeMinFrequencyWithCompletionSelector = mkSelector "readAttributeMinFrequencyWithCompletion:"

-- | @Selector@ for @subscribeAttributeMinFrequencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinFrequencyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinFrequencyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinFrequencyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinFrequencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinFrequencyWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMinFrequencyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinFrequencyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxFrequencyWithCompletion:@
readAttributeMaxFrequencyWithCompletionSelector :: Selector
readAttributeMaxFrequencyWithCompletionSelector = mkSelector "readAttributeMaxFrequencyWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxFrequencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxFrequencyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxFrequencyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxFrequencyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxFrequencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxFrequencyWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxFrequencyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxFrequencyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOptionsWithCompletion:@
readAttributeOptionsWithCompletionSelector :: Selector
readAttributeOptionsWithCompletionSelector = mkSelector "readAttributeOptionsWithCompletion:"

-- | @Selector@ for @writeAttributeOptionsWithValue:completion:@
writeAttributeOptionsWithValue_completionSelector :: Selector
writeAttributeOptionsWithValue_completionSelector = mkSelector "writeAttributeOptionsWithValue:completion:"

-- | @Selector@ for @writeAttributeOptionsWithValue:params:completion:@
writeAttributeOptionsWithValue_params_completionSelector :: Selector
writeAttributeOptionsWithValue_params_completionSelector = mkSelector "writeAttributeOptionsWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOptionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOptionsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOptionsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOptionsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOptionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeOptionsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOptionsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOptionsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOnOffTransitionTimeWithCompletion:@
readAttributeOnOffTransitionTimeWithCompletionSelector :: Selector
readAttributeOnOffTransitionTimeWithCompletionSelector = mkSelector "readAttributeOnOffTransitionTimeWithCompletion:"

-- | @Selector@ for @writeAttributeOnOffTransitionTimeWithValue:completion:@
writeAttributeOnOffTransitionTimeWithValue_completionSelector :: Selector
writeAttributeOnOffTransitionTimeWithValue_completionSelector = mkSelector "writeAttributeOnOffTransitionTimeWithValue:completion:"

-- | @Selector@ for @writeAttributeOnOffTransitionTimeWithValue:params:completion:@
writeAttributeOnOffTransitionTimeWithValue_params_completionSelector :: Selector
writeAttributeOnOffTransitionTimeWithValue_params_completionSelector = mkSelector "writeAttributeOnOffTransitionTimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOnOffTransitionTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffTransitionTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnOffTransitionTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnOffTransitionTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnOffTransitionTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnOffTransitionTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOnOffTransitionTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnOffTransitionTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOnLevelWithCompletion:@
readAttributeOnLevelWithCompletionSelector :: Selector
readAttributeOnLevelWithCompletionSelector = mkSelector "readAttributeOnLevelWithCompletion:"

-- | @Selector@ for @writeAttributeOnLevelWithValue:completion:@
writeAttributeOnLevelWithValue_completionSelector :: Selector
writeAttributeOnLevelWithValue_completionSelector = mkSelector "writeAttributeOnLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeOnLevelWithValue:params:completion:@
writeAttributeOnLevelWithValue_params_completionSelector :: Selector
writeAttributeOnLevelWithValue_params_completionSelector = mkSelector "writeAttributeOnLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOnLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOnLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOnTransitionTimeWithCompletion:@
readAttributeOnTransitionTimeWithCompletionSelector :: Selector
readAttributeOnTransitionTimeWithCompletionSelector = mkSelector "readAttributeOnTransitionTimeWithCompletion:"

-- | @Selector@ for @writeAttributeOnTransitionTimeWithValue:completion:@
writeAttributeOnTransitionTimeWithValue_completionSelector :: Selector
writeAttributeOnTransitionTimeWithValue_completionSelector = mkSelector "writeAttributeOnTransitionTimeWithValue:completion:"

-- | @Selector@ for @writeAttributeOnTransitionTimeWithValue:params:completion:@
writeAttributeOnTransitionTimeWithValue_params_completionSelector :: Selector
writeAttributeOnTransitionTimeWithValue_params_completionSelector = mkSelector "writeAttributeOnTransitionTimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOnTransitionTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTransitionTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnTransitionTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnTransitionTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnTransitionTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnTransitionTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOnTransitionTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnTransitionTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOffTransitionTimeWithCompletion:@
readAttributeOffTransitionTimeWithCompletionSelector :: Selector
readAttributeOffTransitionTimeWithCompletionSelector = mkSelector "readAttributeOffTransitionTimeWithCompletion:"

-- | @Selector@ for @writeAttributeOffTransitionTimeWithValue:completion:@
writeAttributeOffTransitionTimeWithValue_completionSelector :: Selector
writeAttributeOffTransitionTimeWithValue_completionSelector = mkSelector "writeAttributeOffTransitionTimeWithValue:completion:"

-- | @Selector@ for @writeAttributeOffTransitionTimeWithValue:params:completion:@
writeAttributeOffTransitionTimeWithValue_params_completionSelector :: Selector
writeAttributeOffTransitionTimeWithValue_params_completionSelector = mkSelector "writeAttributeOffTransitionTimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOffTransitionTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOffTransitionTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOffTransitionTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOffTransitionTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOffTransitionTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOffTransitionTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOffTransitionTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOffTransitionTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultMoveRateWithCompletion:@
readAttributeDefaultMoveRateWithCompletionSelector :: Selector
readAttributeDefaultMoveRateWithCompletionSelector = mkSelector "readAttributeDefaultMoveRateWithCompletion:"

-- | @Selector@ for @writeAttributeDefaultMoveRateWithValue:completion:@
writeAttributeDefaultMoveRateWithValue_completionSelector :: Selector
writeAttributeDefaultMoveRateWithValue_completionSelector = mkSelector "writeAttributeDefaultMoveRateWithValue:completion:"

-- | @Selector@ for @writeAttributeDefaultMoveRateWithValue:params:completion:@
writeAttributeDefaultMoveRateWithValue_params_completionSelector :: Selector
writeAttributeDefaultMoveRateWithValue_params_completionSelector = mkSelector "writeAttributeDefaultMoveRateWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeDefaultMoveRateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultMoveRateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDefaultMoveRateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultMoveRateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultMoveRateWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultMoveRateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDefaultMoveRateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultMoveRateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStartUpCurrentLevelWithCompletion:@
readAttributeStartUpCurrentLevelWithCompletionSelector :: Selector
readAttributeStartUpCurrentLevelWithCompletionSelector = mkSelector "readAttributeStartUpCurrentLevelWithCompletion:"

-- | @Selector@ for @writeAttributeStartUpCurrentLevelWithValue:completion:@
writeAttributeStartUpCurrentLevelWithValue_completionSelector :: Selector
writeAttributeStartUpCurrentLevelWithValue_completionSelector = mkSelector "writeAttributeStartUpCurrentLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeStartUpCurrentLevelWithValue:params:completion:@
writeAttributeStartUpCurrentLevelWithValue_params_completionSelector :: Selector
writeAttributeStartUpCurrentLevelWithValue_params_completionSelector = mkSelector "writeAttributeStartUpCurrentLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeStartUpCurrentLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStartUpCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartUpCurrentLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartUpCurrentLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartUpCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStartUpCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStartUpCurrentLevelWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @moveToLevelWithParams:completionHandler:@
moveToLevelWithParams_completionHandlerSelector :: Selector
moveToLevelWithParams_completionHandlerSelector = mkSelector "moveToLevelWithParams:completionHandler:"

-- | @Selector@ for @moveWithParams:completionHandler:@
moveWithParams_completionHandlerSelector :: Selector
moveWithParams_completionHandlerSelector = mkSelector "moveWithParams:completionHandler:"

-- | @Selector@ for @stepWithParams:completionHandler:@
stepWithParams_completionHandlerSelector :: Selector
stepWithParams_completionHandlerSelector = mkSelector "stepWithParams:completionHandler:"

-- | @Selector@ for @stopWithParams:completionHandler:@
stopWithParams_completionHandlerSelector :: Selector
stopWithParams_completionHandlerSelector = mkSelector "stopWithParams:completionHandler:"

-- | @Selector@ for @moveToLevelWithOnOffWithParams:completionHandler:@
moveToLevelWithOnOffWithParams_completionHandlerSelector :: Selector
moveToLevelWithOnOffWithParams_completionHandlerSelector = mkSelector "moveToLevelWithOnOffWithParams:completionHandler:"

-- | @Selector@ for @moveWithOnOffWithParams:completionHandler:@
moveWithOnOffWithParams_completionHandlerSelector :: Selector
moveWithOnOffWithParams_completionHandlerSelector = mkSelector "moveWithOnOffWithParams:completionHandler:"

-- | @Selector@ for @stepWithOnOffWithParams:completionHandler:@
stepWithOnOffWithParams_completionHandlerSelector :: Selector
stepWithOnOffWithParams_completionHandlerSelector = mkSelector "stepWithOnOffWithParams:completionHandler:"

-- | @Selector@ for @stopWithOnOffWithParams:completionHandler:@
stopWithOnOffWithParams_completionHandlerSelector :: Selector
stopWithOnOffWithParams_completionHandlerSelector = mkSelector "stopWithOnOffWithParams:completionHandler:"

-- | @Selector@ for @moveToClosestFrequencyWithParams:completionHandler:@
moveToClosestFrequencyWithParams_completionHandlerSelector :: Selector
moveToClosestFrequencyWithParams_completionHandlerSelector = mkSelector "moveToClosestFrequencyWithParams:completionHandler:"

-- | @Selector@ for @readAttributeCurrentLevelWithCompletionHandler:@
readAttributeCurrentLevelWithCompletionHandlerSelector :: Selector
readAttributeCurrentLevelWithCompletionHandlerSelector = mkSelector "readAttributeCurrentLevelWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentLevelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCurrentLevelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentLevelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeRemainingTimeWithCompletionHandler:@
readAttributeRemainingTimeWithCompletionHandlerSelector :: Selector
readAttributeRemainingTimeWithCompletionHandlerSelector = mkSelector "readAttributeRemainingTimeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeRemainingTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRemainingTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRemainingTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRemainingTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRemainingTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeRemainingTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeRemainingTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMinLevelWithCompletionHandler:@
readAttributeMinLevelWithCompletionHandlerSelector :: Selector
readAttributeMinLevelWithCompletionHandlerSelector = mkSelector "readAttributeMinLevelWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMinLevelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMaxLevelWithCompletionHandler:@
readAttributeMaxLevelWithCompletionHandlerSelector :: Selector
readAttributeMaxLevelWithCompletionHandlerSelector = mkSelector "readAttributeMaxLevelWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMaxLevelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentFrequencyWithCompletionHandler:@
readAttributeCurrentFrequencyWithCompletionHandlerSelector :: Selector
readAttributeCurrentFrequencyWithCompletionHandlerSelector = mkSelector "readAttributeCurrentFrequencyWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentFrequencyWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentFrequencyWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCurrentFrequencyWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentFrequencyWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMinFrequencyWithCompletionHandler:@
readAttributeMinFrequencyWithCompletionHandlerSelector :: Selector
readAttributeMinFrequencyWithCompletionHandlerSelector = mkSelector "readAttributeMinFrequencyWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMinFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMinFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinFrequencyWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMinFrequencyWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMinFrequencyWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMinFrequencyWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMaxFrequencyWithCompletionHandler:@
readAttributeMaxFrequencyWithCompletionHandlerSelector :: Selector
readAttributeMaxFrequencyWithCompletionHandlerSelector = mkSelector "readAttributeMaxFrequencyWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMaxFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxFrequencyWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxFrequencyWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxFrequencyWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxFrequencyWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMaxFrequencyWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMaxFrequencyWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOptionsWithCompletionHandler:@
readAttributeOptionsWithCompletionHandlerSelector :: Selector
readAttributeOptionsWithCompletionHandlerSelector = mkSelector "readAttributeOptionsWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOptionsWithValue:completionHandler:@
writeAttributeOptionsWithValue_completionHandlerSelector :: Selector
writeAttributeOptionsWithValue_completionHandlerSelector = mkSelector "writeAttributeOptionsWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOptionsWithValue:params:completionHandler:@
writeAttributeOptionsWithValue_params_completionHandlerSelector :: Selector
writeAttributeOptionsWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOptionsWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOptionsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOptionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOptionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOptionsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOptionsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOptionsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOptionsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOptionsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOnOffTransitionTimeWithCompletionHandler:@
readAttributeOnOffTransitionTimeWithCompletionHandlerSelector :: Selector
readAttributeOnOffTransitionTimeWithCompletionHandlerSelector = mkSelector "readAttributeOnOffTransitionTimeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOnOffTransitionTimeWithValue:completionHandler:@
writeAttributeOnOffTransitionTimeWithValue_completionHandlerSelector :: Selector
writeAttributeOnOffTransitionTimeWithValue_completionHandlerSelector = mkSelector "writeAttributeOnOffTransitionTimeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOnOffTransitionTimeWithValue:params:completionHandler:@
writeAttributeOnOffTransitionTimeWithValue_params_completionHandlerSelector :: Selector
writeAttributeOnOffTransitionTimeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOnOffTransitionTimeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOnOffTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnOffTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnOffTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOnOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOnOffTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOnLevelWithCompletionHandler:@
readAttributeOnLevelWithCompletionHandlerSelector :: Selector
readAttributeOnLevelWithCompletionHandlerSelector = mkSelector "readAttributeOnLevelWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOnLevelWithValue:completionHandler:@
writeAttributeOnLevelWithValue_completionHandlerSelector :: Selector
writeAttributeOnLevelWithValue_completionHandlerSelector = mkSelector "writeAttributeOnLevelWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOnLevelWithValue:params:completionHandler:@
writeAttributeOnLevelWithValue_params_completionHandlerSelector :: Selector
writeAttributeOnLevelWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOnLevelWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOnLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnLevelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOnLevelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOnLevelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOnTransitionTimeWithCompletionHandler:@
readAttributeOnTransitionTimeWithCompletionHandlerSelector :: Selector
readAttributeOnTransitionTimeWithCompletionHandlerSelector = mkSelector "readAttributeOnTransitionTimeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOnTransitionTimeWithValue:completionHandler:@
writeAttributeOnTransitionTimeWithValue_completionHandlerSelector :: Selector
writeAttributeOnTransitionTimeWithValue_completionHandlerSelector = mkSelector "writeAttributeOnTransitionTimeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOnTransitionTimeWithValue:params:completionHandler:@
writeAttributeOnTransitionTimeWithValue_params_completionHandlerSelector :: Selector
writeAttributeOnTransitionTimeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOnTransitionTimeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOnTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnTransitionTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOnTransitionTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOnTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOffTransitionTimeWithCompletionHandler:@
readAttributeOffTransitionTimeWithCompletionHandlerSelector :: Selector
readAttributeOffTransitionTimeWithCompletionHandlerSelector = mkSelector "readAttributeOffTransitionTimeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOffTransitionTimeWithValue:completionHandler:@
writeAttributeOffTransitionTimeWithValue_completionHandlerSelector :: Selector
writeAttributeOffTransitionTimeWithValue_completionHandlerSelector = mkSelector "writeAttributeOffTransitionTimeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOffTransitionTimeWithValue:params:completionHandler:@
writeAttributeOffTransitionTimeWithValue_params_completionHandlerSelector :: Selector
writeAttributeOffTransitionTimeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOffTransitionTimeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOffTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOffTransitionTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOffTransitionTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOffTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOffTransitionTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOffTransitionTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeDefaultMoveRateWithCompletionHandler:@
readAttributeDefaultMoveRateWithCompletionHandlerSelector :: Selector
readAttributeDefaultMoveRateWithCompletionHandlerSelector = mkSelector "readAttributeDefaultMoveRateWithCompletionHandler:"

-- | @Selector@ for @writeAttributeDefaultMoveRateWithValue:completionHandler:@
writeAttributeDefaultMoveRateWithValue_completionHandlerSelector :: Selector
writeAttributeDefaultMoveRateWithValue_completionHandlerSelector = mkSelector "writeAttributeDefaultMoveRateWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeDefaultMoveRateWithValue:params:completionHandler:@
writeAttributeDefaultMoveRateWithValue_params_completionHandlerSelector :: Selector
writeAttributeDefaultMoveRateWithValue_params_completionHandlerSelector = mkSelector "writeAttributeDefaultMoveRateWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeDefaultMoveRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultMoveRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDefaultMoveRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultMoveRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultMoveRateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDefaultMoveRateWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeDefaultMoveRateWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeDefaultMoveRateWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStartUpCurrentLevelWithCompletionHandler:@
readAttributeStartUpCurrentLevelWithCompletionHandlerSelector :: Selector
readAttributeStartUpCurrentLevelWithCompletionHandlerSelector = mkSelector "readAttributeStartUpCurrentLevelWithCompletionHandler:"

-- | @Selector@ for @writeAttributeStartUpCurrentLevelWithValue:completionHandler:@
writeAttributeStartUpCurrentLevelWithValue_completionHandlerSelector :: Selector
writeAttributeStartUpCurrentLevelWithValue_completionHandlerSelector = mkSelector "writeAttributeStartUpCurrentLevelWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeStartUpCurrentLevelWithValue:params:completionHandler:@
writeAttributeStartUpCurrentLevelWithValue_params_completionHandlerSelector :: Selector
writeAttributeStartUpCurrentLevelWithValue_params_completionHandlerSelector = mkSelector "writeAttributeStartUpCurrentLevelWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeStartUpCurrentLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStartUpCurrentLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartUpCurrentLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartUpCurrentLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartUpCurrentLevelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeStartUpCurrentLevelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStartUpCurrentLevelWithAttributeCache:endpoint:queue:completionHandler:"

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

