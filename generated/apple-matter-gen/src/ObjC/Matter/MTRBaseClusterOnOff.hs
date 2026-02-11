{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster On/Off
--
-- Attributes and commands for switching devices between 'On' and 'Off' states.
--
-- Generated bindings for @MTRBaseClusterOnOff@.
module ObjC.Matter.MTRBaseClusterOnOff
  ( MTRBaseClusterOnOff
  , IsMTRBaseClusterOnOff(..)
  , offWithParams_completion
  , offWithCompletion
  , onWithParams_completion
  , onWithCompletion
  , toggleWithParams_completion
  , toggleWithCompletion
  , offWithEffectWithParams_completion
  , onWithRecallGlobalSceneWithParams_completion
  , onWithRecallGlobalSceneWithCompletion
  , onWithTimedOffWithParams_completion
  , readAttributeOnOffWithCompletion
  , subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnOffWithClusterStateCache_endpoint_queue_completion
  , readAttributeGlobalSceneControlWithCompletion
  , subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandler
  , readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completion
  , readAttributeOnTimeWithCompletion
  , writeAttributeOnTimeWithValue_completion
  , writeAttributeOnTimeWithValue_params_completion
  , subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeOffWaitTimeWithCompletion
  , writeAttributeOffWaitTimeWithValue_completion
  , writeAttributeOffWaitTimeWithValue_params_completion
  , subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeStartUpOnOffWithCompletion
  , writeAttributeStartUpOnOffWithValue_completion
  , writeAttributeStartUpOnOffWithValue_params_completion
  , subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandler
  , readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completion
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
  , offWithParams_completionHandler
  , offWithCompletionHandler
  , onWithParams_completionHandler
  , onWithCompletionHandler
  , toggleWithParams_completionHandler
  , toggleWithCompletionHandler
  , offWithEffectWithParams_completionHandler
  , onWithRecallGlobalSceneWithParams_completionHandler
  , onWithRecallGlobalSceneWithCompletionHandler
  , onWithTimedOffWithParams_completionHandler
  , readAttributeOnOffWithCompletionHandler
  , subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeGlobalSceneControlWithCompletionHandler
  , subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOnTimeWithCompletionHandler
  , writeAttributeOnTimeWithValue_completionHandler
  , writeAttributeOnTimeWithValue_params_completionHandler
  , subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOffWaitTimeWithCompletionHandler
  , writeAttributeOffWaitTimeWithValue_completionHandler
  , writeAttributeOffWaitTimeWithValue_params_completionHandler
  , subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStartUpOnOffWithCompletionHandler
  , writeAttributeStartUpOnOffWithValue_completionHandler
  , writeAttributeStartUpOnOffWithValue_params_completionHandler
  , subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandler
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
  , offWithParams_completionSelector
  , offWithCompletionSelector
  , onWithParams_completionSelector
  , onWithCompletionSelector
  , toggleWithParams_completionSelector
  , toggleWithCompletionSelector
  , offWithEffectWithParams_completionSelector
  , onWithRecallGlobalSceneWithParams_completionSelector
  , onWithRecallGlobalSceneWithCompletionSelector
  , onWithTimedOffWithParams_completionSelector
  , readAttributeOnOffWithCompletionSelector
  , subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnOffWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGlobalSceneControlWithCompletionSelector
  , subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnTimeWithCompletionSelector
  , writeAttributeOnTimeWithValue_completionSelector
  , writeAttributeOnTimeWithValue_params_completionSelector
  , subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOffWaitTimeWithCompletionSelector
  , writeAttributeOffWaitTimeWithValue_completionSelector
  , writeAttributeOffWaitTimeWithValue_params_completionSelector
  , subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStartUpOnOffWithCompletionSelector
  , writeAttributeStartUpOnOffWithValue_completionSelector
  , writeAttributeStartUpOnOffWithValue_params_completionSelector
  , subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completionSelector
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
  , offWithParams_completionHandlerSelector
  , offWithCompletionHandlerSelector
  , onWithParams_completionHandlerSelector
  , onWithCompletionHandlerSelector
  , toggleWithParams_completionHandlerSelector
  , toggleWithCompletionHandlerSelector
  , offWithEffectWithParams_completionHandlerSelector
  , onWithRecallGlobalSceneWithParams_completionHandlerSelector
  , onWithRecallGlobalSceneWithCompletionHandlerSelector
  , onWithTimedOffWithParams_completionHandlerSelector
  , readAttributeOnOffWithCompletionHandlerSelector
  , subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGlobalSceneControlWithCompletionHandlerSelector
  , subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOnTimeWithCompletionHandlerSelector
  , writeAttributeOnTimeWithValue_completionHandlerSelector
  , writeAttributeOnTimeWithValue_params_completionHandlerSelector
  , subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOffWaitTimeWithCompletionHandlerSelector
  , writeAttributeOffWaitTimeWithValue_completionHandlerSelector
  , writeAttributeOffWaitTimeWithValue_params_completionHandlerSelector
  , subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStartUpOnOffWithCompletionHandlerSelector
  , writeAttributeStartUpOnOffWithValue_completionHandlerSelector
  , writeAttributeStartUpOnOffWithValue_params_completionHandlerSelector
  , subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command Off
--
-- On receipt of this command, a device SHALL enter its ‘Off’ state. This state is device dependent, but it is recommended that it is used for power off or similar functions. On receipt of the Off command, the OnTime attribute SHALL be set to 0.
--
-- ObjC selector: @- offWithParams:completion:@
offWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOffParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
offWithParams_completion mtrBaseClusterOnOff  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "offWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- offWithCompletion:@
offWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
offWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "offWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command On
--
-- On receipt of this command, a device SHALL enter its ‘On’ state. This state is device dependent, but it is recommended that it is used for power on or similar functions. On receipt of the On command, if the value of the OnTime attribute is equal to 0, the device SHALL set the OffWaitTime attribute to 0.
--
-- ObjC selector: @- onWithParams:completion:@
onWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithParams_completion mtrBaseClusterOnOff  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "onWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- onWithCompletion:@
onWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
onWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "onWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Toggle
--
-- On receipt of this command, if a device is in its ‘Off’ state it SHALL enter its ‘On’ state. Otherwise, if it is in its ‘On’ state it SHALL enter its ‘Off’ state. On receipt of the Toggle command, if the value of the OnOff attribute is equal to FALSE and if the value of the OnTime attribute is equal to 0, the device SHALL set the OffWaitTime attribute to 0. If the value of the OnOff attribute is equal to TRUE, the OnTime attribute SHALL be set to 0.
--
-- ObjC selector: @- toggleWithParams:completion:@
toggleWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterToggleParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
toggleWithParams_completion mtrBaseClusterOnOff  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "toggleWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- toggleWithCompletion:@
toggleWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
toggleWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "toggleWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command OffWithEffect
--
-- The OffWithEffect command allows devices to be turned off using enhanced ways of fading.
--
-- ObjC selector: @- offWithEffectWithParams:completion:@
offWithEffectWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOffWithEffectParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
offWithEffectWithParams_completion mtrBaseClusterOnOff  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "offWithEffectWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command OnWithRecallGlobalScene
--
-- This command allows the recall of the settings when the device was turned off.
--
-- ObjC selector: @- onWithRecallGlobalSceneWithParams:completion:@
onWithRecallGlobalSceneWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnWithRecallGlobalSceneParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithRecallGlobalSceneWithParams_completion mtrBaseClusterOnOff  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "onWithRecallGlobalSceneWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- onWithRecallGlobalSceneWithCompletion:@
onWithRecallGlobalSceneWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
onWithRecallGlobalSceneWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "onWithRecallGlobalSceneWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command OnWithTimedOff
--
-- This command allows devices to be turned on for a specific duration with a guarded off duration so that SHOULD the device be subsequently turned off, further OnWithTimedOff commands, received during this time, are prevented from turning the devices back on.
--
-- ObjC selector: @- onWithTimedOffWithParams:completion:@
onWithTimedOffWithParams_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnWithTimedOffParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithTimedOffWithParams_completion mtrBaseClusterOnOff  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "onWithTimedOffWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOnOffWithCompletion:@
readAttributeOnOffWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOnOffWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeOnOffWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOnOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeOnOffWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnOffWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnOffWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnOffWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGlobalSceneControlWithCompletion:@
readAttributeGlobalSceneControlWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeGlobalSceneControlWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeGlobalSceneControlWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGlobalSceneControlWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeGlobalSceneControlWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGlobalSceneControlWithClusterStateCache:endpoint:queue:completion:@
readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGlobalSceneControlWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOnTimeWithCompletion:@
readAttributeOnTimeWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOnTimeWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeOnTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOnTimeWithValue:completion:@
writeAttributeOnTimeWithValue_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeOnTimeWithValue_completion mtrBaseClusterOnOff  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeOnTimeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOnTimeWithValue:params:completion:@
writeAttributeOnTimeWithValue_params_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeOnTimeWithValue_params_completion mtrBaseClusterOnOff  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeOnTimeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOnTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeOnTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOffWaitTimeWithCompletion:@
readAttributeOffWaitTimeWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOffWaitTimeWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeOffWaitTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOffWaitTimeWithValue:completion:@
writeAttributeOffWaitTimeWithValue_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeOffWaitTimeWithValue_completion mtrBaseClusterOnOff  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeOffWaitTimeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOffWaitTimeWithValue:params:completion:@
writeAttributeOffWaitTimeWithValue_params_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeOffWaitTimeWithValue_params_completion mtrBaseClusterOnOff  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeOffWaitTimeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOffWaitTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeOffWaitTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOffWaitTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOffWaitTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStartUpOnOffWithCompletion:@
readAttributeStartUpOnOffWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeStartUpOnOffWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeStartUpOnOffWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeStartUpOnOffWithValue:completion:@
writeAttributeStartUpOnOffWithValue_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeStartUpOnOffWithValue_completion mtrBaseClusterOnOff  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeStartUpOnOffWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeStartUpOnOffWithValue:params:completion:@
writeAttributeStartUpOnOffWithValue_params_completion :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeStartUpOnOffWithValue_params_completion mtrBaseClusterOnOff  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeStartUpOnOffWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStartUpOnOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeStartUpOnOffWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStartUpOnOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStartUpOnOffWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterOnOff  completion =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> IO (Id MTRBaseClusterOnOff)
init_ mtrBaseClusterOnOff  =
    sendMsg mtrBaseClusterOnOff (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterOnOff)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterOnOff -> device -> CUShort -> queue -> IO (Id MTRBaseClusterOnOff)
initWithDevice_endpoint_queue mtrBaseClusterOnOff  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterOnOff (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- offWithParams:completionHandler:@
offWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOffParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
offWithParams_completionHandler mtrBaseClusterOnOff  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "offWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- offWithCompletionHandler:@
offWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
offWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "offWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- onWithParams:completionHandler:@
onWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithParams_completionHandler mtrBaseClusterOnOff  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "onWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- onWithCompletionHandler:@
onWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
onWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "onWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- toggleWithParams:completionHandler:@
toggleWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterToggleParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
toggleWithParams_completionHandler mtrBaseClusterOnOff  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "toggleWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- toggleWithCompletionHandler:@
toggleWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
toggleWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "toggleWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- offWithEffectWithParams:completionHandler:@
offWithEffectWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOffWithEffectParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
offWithEffectWithParams_completionHandler mtrBaseClusterOnOff  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "offWithEffectWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- onWithRecallGlobalSceneWithParams:completionHandler:@
onWithRecallGlobalSceneWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnWithRecallGlobalSceneParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithRecallGlobalSceneWithParams_completionHandler mtrBaseClusterOnOff  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "onWithRecallGlobalSceneWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- onWithRecallGlobalSceneWithCompletionHandler:@
onWithRecallGlobalSceneWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
onWithRecallGlobalSceneWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "onWithRecallGlobalSceneWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- onWithTimedOffWithParams:completionHandler:@
onWithTimedOffWithParams_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTROnOffClusterOnWithTimedOffParams params) => mtrBaseClusterOnOff -> params -> Ptr () -> IO ()
onWithTimedOffWithParams_completionHandler mtrBaseClusterOnOff  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOff (mkSelector "onWithTimedOffWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOnOffWithCompletionHandler:@
readAttributeOnOffWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOnOffWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeOnOffWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnOffWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnOffWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGlobalSceneControlWithCompletionHandler:@
readAttributeGlobalSceneControlWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeGlobalSceneControlWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeGlobalSceneControlWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGlobalSceneControlWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeGlobalSceneControlWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGlobalSceneControlWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGlobalSceneControlWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOnTimeWithCompletionHandler:@
readAttributeOnTimeWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOnTimeWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeOnTimeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOnTimeWithValue:completionHandler:@
writeAttributeOnTimeWithValue_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeOnTimeWithValue_completionHandler mtrBaseClusterOnOff  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeOnTimeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOnTimeWithValue:params:completionHandler:@
writeAttributeOnTimeWithValue_params_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeOnTimeWithValue_params_completionHandler mtrBaseClusterOnOff  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeOnTimeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOnTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeOnTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnTimeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOffWaitTimeWithCompletionHandler:@
readAttributeOffWaitTimeWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeOffWaitTimeWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeOffWaitTimeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOffWaitTimeWithValue:completionHandler:@
writeAttributeOffWaitTimeWithValue_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeOffWaitTimeWithValue_completionHandler mtrBaseClusterOnOff  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeOffWaitTimeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOffWaitTimeWithValue:params:completionHandler:@
writeAttributeOffWaitTimeWithValue_params_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeOffWaitTimeWithValue_params_completionHandler mtrBaseClusterOnOff  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeOffWaitTimeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOffWaitTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeOffWaitTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOffWaitTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOffWaitTimeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeStartUpOnOffWithCompletionHandler:@
readAttributeStartUpOnOffWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeStartUpOnOffWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeStartUpOnOffWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeStartUpOnOffWithValue:completionHandler:@
writeAttributeStartUpOnOffWithValue_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value) => mtrBaseClusterOnOff -> value -> Ptr () -> IO ()
writeAttributeStartUpOnOffWithValue_completionHandler mtrBaseClusterOnOff  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeStartUpOnOffWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeStartUpOnOffWithValue:params:completionHandler:@
writeAttributeStartUpOnOffWithValue_params_completionHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOff -> value -> params -> Ptr () -> IO ()
writeAttributeStartUpOnOffWithValue_params_completionHandler mtrBaseClusterOnOff  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterOnOff (mkSelector "writeAttributeStartUpOnOffWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeStartUpOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeStartUpOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStartUpOnOffWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStartUpOnOffWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterOnOff mtrBaseClusterOnOff => mtrBaseClusterOnOff -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterOnOff  completionHandler =
    sendMsg mtrBaseClusterOnOff (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOff -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOff  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOff (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOff"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterOnOff mtrBaseClusterOnOff, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterOnOff -> device -> endpointID -> queue -> IO (Id MTRBaseClusterOnOff)
initWithDevice_endpointID_queue mtrBaseClusterOnOff  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterOnOff (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offWithParams:completion:@
offWithParams_completionSelector :: Selector
offWithParams_completionSelector = mkSelector "offWithParams:completion:"

-- | @Selector@ for @offWithCompletion:@
offWithCompletionSelector :: Selector
offWithCompletionSelector = mkSelector "offWithCompletion:"

-- | @Selector@ for @onWithParams:completion:@
onWithParams_completionSelector :: Selector
onWithParams_completionSelector = mkSelector "onWithParams:completion:"

-- | @Selector@ for @onWithCompletion:@
onWithCompletionSelector :: Selector
onWithCompletionSelector = mkSelector "onWithCompletion:"

-- | @Selector@ for @toggleWithParams:completion:@
toggleWithParams_completionSelector :: Selector
toggleWithParams_completionSelector = mkSelector "toggleWithParams:completion:"

-- | @Selector@ for @toggleWithCompletion:@
toggleWithCompletionSelector :: Selector
toggleWithCompletionSelector = mkSelector "toggleWithCompletion:"

-- | @Selector@ for @offWithEffectWithParams:completion:@
offWithEffectWithParams_completionSelector :: Selector
offWithEffectWithParams_completionSelector = mkSelector "offWithEffectWithParams:completion:"

-- | @Selector@ for @onWithRecallGlobalSceneWithParams:completion:@
onWithRecallGlobalSceneWithParams_completionSelector :: Selector
onWithRecallGlobalSceneWithParams_completionSelector = mkSelector "onWithRecallGlobalSceneWithParams:completion:"

-- | @Selector@ for @onWithRecallGlobalSceneWithCompletion:@
onWithRecallGlobalSceneWithCompletionSelector :: Selector
onWithRecallGlobalSceneWithCompletionSelector = mkSelector "onWithRecallGlobalSceneWithCompletion:"

-- | @Selector@ for @onWithTimedOffWithParams:completion:@
onWithTimedOffWithParams_completionSelector :: Selector
onWithTimedOffWithParams_completionSelector = mkSelector "onWithTimedOffWithParams:completion:"

-- | @Selector@ for @readAttributeOnOffWithCompletion:@
readAttributeOnOffWithCompletionSelector :: Selector
readAttributeOnOffWithCompletionSelector = mkSelector "readAttributeOnOffWithCompletion:"

-- | @Selector@ for @subscribeAttributeOnOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnOffWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnOffWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnOffWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOnOffWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnOffWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGlobalSceneControlWithCompletion:@
readAttributeGlobalSceneControlWithCompletionSelector :: Selector
readAttributeGlobalSceneControlWithCompletionSelector = mkSelector "readAttributeGlobalSceneControlWithCompletion:"

-- | @Selector@ for @subscribeAttributeGlobalSceneControlWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGlobalSceneControlWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGlobalSceneControlWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGlobalSceneControlWithClusterStateCache:endpoint:queue:completion:@
readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeGlobalSceneControlWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGlobalSceneControlWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOnTimeWithCompletion:@
readAttributeOnTimeWithCompletionSelector :: Selector
readAttributeOnTimeWithCompletionSelector = mkSelector "readAttributeOnTimeWithCompletion:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:completion:@
writeAttributeOnTimeWithValue_completionSelector :: Selector
writeAttributeOnTimeWithValue_completionSelector = mkSelector "writeAttributeOnTimeWithValue:completion:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:params:completion:@
writeAttributeOnTimeWithValue_params_completionSelector :: Selector
writeAttributeOnTimeWithValue_params_completionSelector = mkSelector "writeAttributeOnTimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOnTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOnTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOffWaitTimeWithCompletion:@
readAttributeOffWaitTimeWithCompletionSelector :: Selector
readAttributeOffWaitTimeWithCompletionSelector = mkSelector "readAttributeOffWaitTimeWithCompletion:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:completion:@
writeAttributeOffWaitTimeWithValue_completionSelector :: Selector
writeAttributeOffWaitTimeWithValue_completionSelector = mkSelector "writeAttributeOffWaitTimeWithValue:completion:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:params:completion:@
writeAttributeOffWaitTimeWithValue_params_completionSelector :: Selector
writeAttributeOffWaitTimeWithValue_params_completionSelector = mkSelector "writeAttributeOffWaitTimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOffWaitTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOffWaitTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOffWaitTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOffWaitTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOffWaitTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOffWaitTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStartUpOnOffWithCompletion:@
readAttributeStartUpOnOffWithCompletionSelector :: Selector
readAttributeStartUpOnOffWithCompletionSelector = mkSelector "readAttributeStartUpOnOffWithCompletion:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:completion:@
writeAttributeStartUpOnOffWithValue_completionSelector :: Selector
writeAttributeStartUpOnOffWithValue_completionSelector = mkSelector "writeAttributeStartUpOnOffWithValue:completion:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:params:completion:@
writeAttributeStartUpOnOffWithValue_params_completionSelector :: Selector
writeAttributeStartUpOnOffWithValue_params_completionSelector = mkSelector "writeAttributeStartUpOnOffWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeStartUpOnOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStartUpOnOffWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartUpOnOffWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartUpOnOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStartUpOnOffWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStartUpOnOffWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @offWithParams:completionHandler:@
offWithParams_completionHandlerSelector :: Selector
offWithParams_completionHandlerSelector = mkSelector "offWithParams:completionHandler:"

-- | @Selector@ for @offWithCompletionHandler:@
offWithCompletionHandlerSelector :: Selector
offWithCompletionHandlerSelector = mkSelector "offWithCompletionHandler:"

-- | @Selector@ for @onWithParams:completionHandler:@
onWithParams_completionHandlerSelector :: Selector
onWithParams_completionHandlerSelector = mkSelector "onWithParams:completionHandler:"

-- | @Selector@ for @onWithCompletionHandler:@
onWithCompletionHandlerSelector :: Selector
onWithCompletionHandlerSelector = mkSelector "onWithCompletionHandler:"

-- | @Selector@ for @toggleWithParams:completionHandler:@
toggleWithParams_completionHandlerSelector :: Selector
toggleWithParams_completionHandlerSelector = mkSelector "toggleWithParams:completionHandler:"

-- | @Selector@ for @toggleWithCompletionHandler:@
toggleWithCompletionHandlerSelector :: Selector
toggleWithCompletionHandlerSelector = mkSelector "toggleWithCompletionHandler:"

-- | @Selector@ for @offWithEffectWithParams:completionHandler:@
offWithEffectWithParams_completionHandlerSelector :: Selector
offWithEffectWithParams_completionHandlerSelector = mkSelector "offWithEffectWithParams:completionHandler:"

-- | @Selector@ for @onWithRecallGlobalSceneWithParams:completionHandler:@
onWithRecallGlobalSceneWithParams_completionHandlerSelector :: Selector
onWithRecallGlobalSceneWithParams_completionHandlerSelector = mkSelector "onWithRecallGlobalSceneWithParams:completionHandler:"

-- | @Selector@ for @onWithRecallGlobalSceneWithCompletionHandler:@
onWithRecallGlobalSceneWithCompletionHandlerSelector :: Selector
onWithRecallGlobalSceneWithCompletionHandlerSelector = mkSelector "onWithRecallGlobalSceneWithCompletionHandler:"

-- | @Selector@ for @onWithTimedOffWithParams:completionHandler:@
onWithTimedOffWithParams_completionHandlerSelector :: Selector
onWithTimedOffWithParams_completionHandlerSelector = mkSelector "onWithTimedOffWithParams:completionHandler:"

-- | @Selector@ for @readAttributeOnOffWithCompletionHandler:@
readAttributeOnOffWithCompletionHandlerSelector :: Selector
readAttributeOnOffWithCompletionHandlerSelector = mkSelector "readAttributeOnOffWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnOffWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOnOffWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeGlobalSceneControlWithCompletionHandler:@
readAttributeGlobalSceneControlWithCompletionHandlerSelector :: Selector
readAttributeGlobalSceneControlWithCompletionHandlerSelector = mkSelector "readAttributeGlobalSceneControlWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeGlobalSceneControlWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGlobalSceneControlWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGlobalSceneControlWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGlobalSceneControlWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeGlobalSceneControlWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGlobalSceneControlWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOnTimeWithCompletionHandler:@
readAttributeOnTimeWithCompletionHandlerSelector :: Selector
readAttributeOnTimeWithCompletionHandlerSelector = mkSelector "readAttributeOnTimeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:completionHandler:@
writeAttributeOnTimeWithValue_completionHandlerSelector :: Selector
writeAttributeOnTimeWithValue_completionHandlerSelector = mkSelector "writeAttributeOnTimeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:params:completionHandler:@
writeAttributeOnTimeWithValue_params_completionHandlerSelector :: Selector
writeAttributeOnTimeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOnTimeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOnTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOnTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOnTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOffWaitTimeWithCompletionHandler:@
readAttributeOffWaitTimeWithCompletionHandlerSelector :: Selector
readAttributeOffWaitTimeWithCompletionHandlerSelector = mkSelector "readAttributeOffWaitTimeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:completionHandler:@
writeAttributeOffWaitTimeWithValue_completionHandlerSelector :: Selector
writeAttributeOffWaitTimeWithValue_completionHandlerSelector = mkSelector "writeAttributeOffWaitTimeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:params:completionHandler:@
writeAttributeOffWaitTimeWithValue_params_completionHandlerSelector :: Selector
writeAttributeOffWaitTimeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOffWaitTimeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOffWaitTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOffWaitTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOffWaitTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOffWaitTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOffWaitTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOffWaitTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStartUpOnOffWithCompletionHandler:@
readAttributeStartUpOnOffWithCompletionHandlerSelector :: Selector
readAttributeStartUpOnOffWithCompletionHandlerSelector = mkSelector "readAttributeStartUpOnOffWithCompletionHandler:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:completionHandler:@
writeAttributeStartUpOnOffWithValue_completionHandlerSelector :: Selector
writeAttributeStartUpOnOffWithValue_completionHandlerSelector = mkSelector "writeAttributeStartUpOnOffWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:params:completionHandler:@
writeAttributeStartUpOnOffWithValue_params_completionHandlerSelector :: Selector
writeAttributeStartUpOnOffWithValue_params_completionHandlerSelector = mkSelector "writeAttributeStartUpOnOffWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeStartUpOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStartUpOnOffWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartUpOnOffWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartUpOnOffWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeStartUpOnOffWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStartUpOnOffWithAttributeCache:endpoint:queue:completionHandler:"

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

