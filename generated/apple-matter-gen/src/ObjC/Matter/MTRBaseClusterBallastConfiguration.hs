{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Ballast Configuration
--
-- Attributes and commands for configuring a lighting ballast.
--
-- Generated bindings for @MTRBaseClusterBallastConfiguration@.
module ObjC.Matter.MTRBaseClusterBallastConfiguration
  ( MTRBaseClusterBallastConfiguration
  , IsMTRBaseClusterBallastConfiguration(..)
  , readAttributePhysicalMinLevelWithCompletion
  , subscribeAttributePhysicalMinLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributePhysicalMinLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributePhysicalMaxLevelWithCompletion
  , subscribeAttributePhysicalMaxLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributePhysicalMaxLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeBallastStatusWithCompletion
  , subscribeAttributeBallastStatusWithParams_subscriptionEstablished_reportHandler
  , readAttributeBallastStatusWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinLevelWithCompletion
  , writeAttributeMinLevelWithValue_completion
  , writeAttributeMinLevelWithValue_params_completion
  , subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxLevelWithCompletion
  , writeAttributeMaxLevelWithValue_completion
  , writeAttributeMaxLevelWithValue_params_completion
  , subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeIntrinsicBallastFactorWithCompletion
  , writeAttributeIntrinsicBallastFactorWithValue_completion
  , writeAttributeIntrinsicBallastFactorWithValue_params_completion
  , subscribeAttributeIntrinsicBallastFactorWithParams_subscriptionEstablished_reportHandler
  , readAttributeIntrinsicBallastFactorWithClusterStateCache_endpoint_queue_completion
  , readAttributeBallastFactorAdjustmentWithCompletion
  , writeAttributeBallastFactorAdjustmentWithValue_completion
  , writeAttributeBallastFactorAdjustmentWithValue_params_completion
  , subscribeAttributeBallastFactorAdjustmentWithParams_subscriptionEstablished_reportHandler
  , readAttributeBallastFactorAdjustmentWithClusterStateCache_endpoint_queue_completion
  , readAttributeLampQuantityWithCompletion
  , subscribeAttributeLampQuantityWithParams_subscriptionEstablished_reportHandler
  , readAttributeLampQuantityWithClusterStateCache_endpoint_queue_completion
  , readAttributeLampTypeWithCompletion
  , writeAttributeLampTypeWithValue_completion
  , writeAttributeLampTypeWithValue_params_completion
  , subscribeAttributeLampTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeLampTypeWithClusterStateCache_endpoint_queue_completion
  , readAttributeLampManufacturerWithCompletion
  , writeAttributeLampManufacturerWithValue_completion
  , writeAttributeLampManufacturerWithValue_params_completion
  , subscribeAttributeLampManufacturerWithParams_subscriptionEstablished_reportHandler
  , readAttributeLampManufacturerWithClusterStateCache_endpoint_queue_completion
  , readAttributeLampRatedHoursWithCompletion
  , writeAttributeLampRatedHoursWithValue_completion
  , writeAttributeLampRatedHoursWithValue_params_completion
  , subscribeAttributeLampRatedHoursWithParams_subscriptionEstablished_reportHandler
  , readAttributeLampRatedHoursWithClusterStateCache_endpoint_queue_completion
  , readAttributeLampBurnHoursWithCompletion
  , writeAttributeLampBurnHoursWithValue_completion
  , writeAttributeLampBurnHoursWithValue_params_completion
  , subscribeAttributeLampBurnHoursWithParams_subscriptionEstablished_reportHandler
  , readAttributeLampBurnHoursWithClusterStateCache_endpoint_queue_completion
  , readAttributeLampAlarmModeWithCompletion
  , writeAttributeLampAlarmModeWithValue_completion
  , writeAttributeLampAlarmModeWithValue_params_completion
  , subscribeAttributeLampAlarmModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeLampAlarmModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeLampBurnHoursTripPointWithCompletion
  , writeAttributeLampBurnHoursTripPointWithValue_completion
  , writeAttributeLampBurnHoursTripPointWithValue_params_completion
  , subscribeAttributeLampBurnHoursTripPointWithParams_subscriptionEstablished_reportHandler
  , readAttributeLampBurnHoursTripPointWithClusterStateCache_endpoint_queue_completion
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
  , readAttributePhysicalMinLevelWithCompletionHandler
  , subscribeAttributePhysicalMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePhysicalMinLevelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePhysicalMaxLevelWithCompletionHandler
  , subscribeAttributePhysicalMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePhysicalMaxLevelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBallastStatusWithCompletionHandler
  , subscribeAttributeBallastStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBallastStatusWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMinLevelWithCompletionHandler
  , writeAttributeMinLevelWithValue_completionHandler
  , writeAttributeMinLevelWithValue_params_completionHandler
  , subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMaxLevelWithCompletionHandler
  , writeAttributeMaxLevelWithValue_completionHandler
  , writeAttributeMaxLevelWithValue_params_completionHandler
  , subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeIntrinsicBalanceFactorWithCompletionHandler
  , writeAttributeIntrinsicBalanceFactorWithValue_completionHandler
  , writeAttributeIntrinsicBalanceFactorWithValue_params_completionHandler
  , subscribeAttributeIntrinsicBalanceFactorWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeIntrinsicBalanceFactorWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBallastFactorAdjustmentWithCompletionHandler
  , writeAttributeBallastFactorAdjustmentWithValue_completionHandler
  , writeAttributeBallastFactorAdjustmentWithValue_params_completionHandler
  , subscribeAttributeBallastFactorAdjustmentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBallastFactorAdjustmentWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLampQuantityWithCompletionHandler
  , subscribeAttributeLampQuantityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLampQuantityWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLampTypeWithCompletionHandler
  , writeAttributeLampTypeWithValue_completionHandler
  , writeAttributeLampTypeWithValue_params_completionHandler
  , subscribeAttributeLampTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLampTypeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLampManufacturerWithCompletionHandler
  , writeAttributeLampManufacturerWithValue_completionHandler
  , writeAttributeLampManufacturerWithValue_params_completionHandler
  , subscribeAttributeLampManufacturerWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLampManufacturerWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLampRatedHoursWithCompletionHandler
  , writeAttributeLampRatedHoursWithValue_completionHandler
  , writeAttributeLampRatedHoursWithValue_params_completionHandler
  , subscribeAttributeLampRatedHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLampRatedHoursWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLampBurnHoursWithCompletionHandler
  , writeAttributeLampBurnHoursWithValue_completionHandler
  , writeAttributeLampBurnHoursWithValue_params_completionHandler
  , subscribeAttributeLampBurnHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLampBurnHoursWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLampAlarmModeWithCompletionHandler
  , writeAttributeLampAlarmModeWithValue_completionHandler
  , writeAttributeLampAlarmModeWithValue_params_completionHandler
  , subscribeAttributeLampAlarmModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLampAlarmModeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLampBurnHoursTripPointWithCompletionHandler
  , writeAttributeLampBurnHoursTripPointWithValue_completionHandler
  , writeAttributeLampBurnHoursTripPointWithValue_params_completionHandler
  , subscribeAttributeLampBurnHoursTripPointWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLampBurnHoursTripPointWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributePhysicalMinLevelWithCompletionSelector
  , subscribeAttributePhysicalMinLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePhysicalMinLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePhysicalMaxLevelWithCompletionSelector
  , subscribeAttributePhysicalMaxLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePhysicalMaxLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBallastStatusWithCompletionSelector
  , subscribeAttributeBallastStatusWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBallastStatusWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinLevelWithCompletionSelector
  , writeAttributeMinLevelWithValue_completionSelector
  , writeAttributeMinLevelWithValue_params_completionSelector
  , subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxLevelWithCompletionSelector
  , writeAttributeMaxLevelWithValue_completionSelector
  , writeAttributeMaxLevelWithValue_params_completionSelector
  , subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeIntrinsicBallastFactorWithCompletionSelector
  , writeAttributeIntrinsicBallastFactorWithValue_completionSelector
  , writeAttributeIntrinsicBallastFactorWithValue_params_completionSelector
  , subscribeAttributeIntrinsicBallastFactorWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeIntrinsicBallastFactorWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBallastFactorAdjustmentWithCompletionSelector
  , writeAttributeBallastFactorAdjustmentWithValue_completionSelector
  , writeAttributeBallastFactorAdjustmentWithValue_params_completionSelector
  , subscribeAttributeBallastFactorAdjustmentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBallastFactorAdjustmentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLampQuantityWithCompletionSelector
  , subscribeAttributeLampQuantityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampQuantityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLampTypeWithCompletionSelector
  , writeAttributeLampTypeWithValue_completionSelector
  , writeAttributeLampTypeWithValue_params_completionSelector
  , subscribeAttributeLampTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLampManufacturerWithCompletionSelector
  , writeAttributeLampManufacturerWithValue_completionSelector
  , writeAttributeLampManufacturerWithValue_params_completionSelector
  , subscribeAttributeLampManufacturerWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampManufacturerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLampRatedHoursWithCompletionSelector
  , writeAttributeLampRatedHoursWithValue_completionSelector
  , writeAttributeLampRatedHoursWithValue_params_completionSelector
  , subscribeAttributeLampRatedHoursWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampRatedHoursWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLampBurnHoursWithCompletionSelector
  , writeAttributeLampBurnHoursWithValue_completionSelector
  , writeAttributeLampBurnHoursWithValue_params_completionSelector
  , subscribeAttributeLampBurnHoursWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampBurnHoursWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLampAlarmModeWithCompletionSelector
  , writeAttributeLampAlarmModeWithValue_completionSelector
  , writeAttributeLampAlarmModeWithValue_params_completionSelector
  , subscribeAttributeLampAlarmModeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampAlarmModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLampBurnHoursTripPointWithCompletionSelector
  , writeAttributeLampBurnHoursTripPointWithValue_completionSelector
  , writeAttributeLampBurnHoursTripPointWithValue_params_completionSelector
  , subscribeAttributeLampBurnHoursTripPointWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampBurnHoursTripPointWithClusterStateCache_endpoint_queue_completionSelector
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
  , readAttributePhysicalMinLevelWithCompletionHandlerSelector
  , subscribeAttributePhysicalMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePhysicalMinLevelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePhysicalMaxLevelWithCompletionHandlerSelector
  , subscribeAttributePhysicalMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePhysicalMaxLevelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBallastStatusWithCompletionHandlerSelector
  , subscribeAttributeBallastStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBallastStatusWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMinLevelWithCompletionHandlerSelector
  , writeAttributeMinLevelWithValue_completionHandlerSelector
  , writeAttributeMinLevelWithValue_params_completionHandlerSelector
  , subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMaxLevelWithCompletionHandlerSelector
  , writeAttributeMaxLevelWithValue_completionHandlerSelector
  , writeAttributeMaxLevelWithValue_params_completionHandlerSelector
  , subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeIntrinsicBalanceFactorWithCompletionHandlerSelector
  , writeAttributeIntrinsicBalanceFactorWithValue_completionHandlerSelector
  , writeAttributeIntrinsicBalanceFactorWithValue_params_completionHandlerSelector
  , subscribeAttributeIntrinsicBalanceFactorWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeIntrinsicBalanceFactorWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBallastFactorAdjustmentWithCompletionHandlerSelector
  , writeAttributeBallastFactorAdjustmentWithValue_completionHandlerSelector
  , writeAttributeBallastFactorAdjustmentWithValue_params_completionHandlerSelector
  , subscribeAttributeBallastFactorAdjustmentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBallastFactorAdjustmentWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLampQuantityWithCompletionHandlerSelector
  , subscribeAttributeLampQuantityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampQuantityWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLampTypeWithCompletionHandlerSelector
  , writeAttributeLampTypeWithValue_completionHandlerSelector
  , writeAttributeLampTypeWithValue_params_completionHandlerSelector
  , subscribeAttributeLampTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampTypeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLampManufacturerWithCompletionHandlerSelector
  , writeAttributeLampManufacturerWithValue_completionHandlerSelector
  , writeAttributeLampManufacturerWithValue_params_completionHandlerSelector
  , subscribeAttributeLampManufacturerWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampManufacturerWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLampRatedHoursWithCompletionHandlerSelector
  , writeAttributeLampRatedHoursWithValue_completionHandlerSelector
  , writeAttributeLampRatedHoursWithValue_params_completionHandlerSelector
  , subscribeAttributeLampRatedHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampRatedHoursWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLampBurnHoursWithCompletionHandlerSelector
  , writeAttributeLampBurnHoursWithValue_completionHandlerSelector
  , writeAttributeLampBurnHoursWithValue_params_completionHandlerSelector
  , subscribeAttributeLampBurnHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampBurnHoursWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLampAlarmModeWithCompletionHandlerSelector
  , writeAttributeLampAlarmModeWithValue_completionHandlerSelector
  , writeAttributeLampAlarmModeWithValue_params_completionHandlerSelector
  , subscribeAttributeLampAlarmModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampAlarmModeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLampBurnHoursTripPointWithCompletionHandlerSelector
  , writeAttributeLampBurnHoursTripPointWithValue_completionHandlerSelector
  , writeAttributeLampBurnHoursTripPointWithValue_params_completionHandlerSelector
  , subscribeAttributeLampBurnHoursTripPointWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLampBurnHoursTripPointWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | @- readAttributePhysicalMinLevelWithCompletion:@
readAttributePhysicalMinLevelWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributePhysicalMinLevelWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributePhysicalMinLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePhysicalMinLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePhysicalMinLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePhysicalMinLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributePhysicalMinLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePhysicalMinLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributePhysicalMinLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePhysicalMinLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePhysicalMinLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePhysicalMaxLevelWithCompletion:@
readAttributePhysicalMaxLevelWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributePhysicalMaxLevelWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributePhysicalMaxLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePhysicalMaxLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePhysicalMaxLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePhysicalMaxLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributePhysicalMaxLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePhysicalMaxLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributePhysicalMaxLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePhysicalMaxLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePhysicalMaxLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBallastStatusWithCompletion:@
readAttributeBallastStatusWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeBallastStatusWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeBallastStatusWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBallastStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBallastStatusWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBallastStatusWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeBallastStatusWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBallastStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeBallastStatusWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBallastStatusWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBallastStatusWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMinLevelWithCompletion:@
readAttributeMinLevelWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeMinLevelWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeMinLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeMinLevelWithValue:completion:@
writeAttributeMinLevelWithValue_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeMinLevelWithValue_completion mtrBaseClusterBallastConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeMinLevelWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeMinLevelWithValue:params:completion:@
writeAttributeMinLevelWithValue_params_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeMinLevelWithValue_params_completion mtrBaseClusterBallastConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeMinLevelWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMinLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeMinLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxLevelWithCompletion:@
readAttributeMaxLevelWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeMaxLevelWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeMaxLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeMaxLevelWithValue:completion:@
writeAttributeMaxLevelWithValue_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeMaxLevelWithValue_completion mtrBaseClusterBallastConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeMaxLevelWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeMaxLevelWithValue:params:completion:@
writeAttributeMaxLevelWithValue_params_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeMaxLevelWithValue_params_completion mtrBaseClusterBallastConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeMaxLevelWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeMaxLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeIntrinsicBallastFactorWithCompletion:@
readAttributeIntrinsicBallastFactorWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeIntrinsicBallastFactorWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeIntrinsicBallastFactorWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeIntrinsicBallastFactorWithValue:completion:@
writeAttributeIntrinsicBallastFactorWithValue_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeIntrinsicBallastFactorWithValue_completion mtrBaseClusterBallastConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeIntrinsicBallastFactorWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeIntrinsicBallastFactorWithValue:params:completion:@
writeAttributeIntrinsicBallastFactorWithValue_params_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeIntrinsicBallastFactorWithValue_params_completion mtrBaseClusterBallastConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeIntrinsicBallastFactorWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeIntrinsicBallastFactorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIntrinsicBallastFactorWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeIntrinsicBallastFactorWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeIntrinsicBallastFactorWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeIntrinsicBallastFactorWithClusterStateCache:endpoint:queue:completion:@
readAttributeIntrinsicBallastFactorWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeIntrinsicBallastFactorWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeIntrinsicBallastFactorWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBallastFactorAdjustmentWithCompletion:@
readAttributeBallastFactorAdjustmentWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeBallastFactorAdjustmentWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeBallastFactorAdjustmentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBallastFactorAdjustmentWithValue:completion:@
writeAttributeBallastFactorAdjustmentWithValue_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeBallastFactorAdjustmentWithValue_completion mtrBaseClusterBallastConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeBallastFactorAdjustmentWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBallastFactorAdjustmentWithValue:params:completion:@
writeAttributeBallastFactorAdjustmentWithValue_params_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeBallastFactorAdjustmentWithValue_params_completion mtrBaseClusterBallastConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeBallastFactorAdjustmentWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBallastFactorAdjustmentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBallastFactorAdjustmentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBallastFactorAdjustmentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeBallastFactorAdjustmentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBallastFactorAdjustmentWithClusterStateCache:endpoint:queue:completion:@
readAttributeBallastFactorAdjustmentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBallastFactorAdjustmentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBallastFactorAdjustmentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLampQuantityWithCompletion:@
readAttributeLampQuantityWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampQuantityWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampQuantityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLampQuantityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampQuantityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampQuantityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampQuantityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampQuantityWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampQuantityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampQuantityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampQuantityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLampTypeWithCompletion:@
readAttributeLampTypeWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampTypeWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampTypeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampTypeWithValue:completion:@
writeAttributeLampTypeWithValue_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSString value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampTypeWithValue_completion mtrBaseClusterBallastConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampTypeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampTypeWithValue:params:completion:@
writeAttributeLampTypeWithValue_params_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampTypeWithValue_params_completion mtrBaseClusterBallastConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampTypeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLampTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampTypeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampTypeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLampManufacturerWithCompletion:@
readAttributeLampManufacturerWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampManufacturerWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampManufacturerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampManufacturerWithValue:completion:@
writeAttributeLampManufacturerWithValue_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSString value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampManufacturerWithValue_completion mtrBaseClusterBallastConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampManufacturerWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampManufacturerWithValue:params:completion:@
writeAttributeLampManufacturerWithValue_params_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampManufacturerWithValue_params_completion mtrBaseClusterBallastConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampManufacturerWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLampManufacturerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampManufacturerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampManufacturerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampManufacturerWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampManufacturerWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampManufacturerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampManufacturerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampManufacturerWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLampRatedHoursWithCompletion:@
readAttributeLampRatedHoursWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampRatedHoursWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampRatedHoursWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampRatedHoursWithValue:completion:@
writeAttributeLampRatedHoursWithValue_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampRatedHoursWithValue_completion mtrBaseClusterBallastConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampRatedHoursWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampRatedHoursWithValue:params:completion:@
writeAttributeLampRatedHoursWithValue_params_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampRatedHoursWithValue_params_completion mtrBaseClusterBallastConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampRatedHoursWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLampRatedHoursWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampRatedHoursWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampRatedHoursWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampRatedHoursWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampRatedHoursWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampRatedHoursWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampRatedHoursWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampRatedHoursWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLampBurnHoursWithCompletion:@
readAttributeLampBurnHoursWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampBurnHoursWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampBurnHoursWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampBurnHoursWithValue:completion:@
writeAttributeLampBurnHoursWithValue_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampBurnHoursWithValue_completion mtrBaseClusterBallastConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampBurnHoursWithValue:params:completion:@
writeAttributeLampBurnHoursWithValue_params_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampBurnHoursWithValue_params_completion mtrBaseClusterBallastConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLampBurnHoursWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampBurnHoursWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampBurnHoursWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampBurnHoursWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampBurnHoursWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampBurnHoursWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampBurnHoursWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampBurnHoursWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLampAlarmModeWithCompletion:@
readAttributeLampAlarmModeWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampAlarmModeWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampAlarmModeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampAlarmModeWithValue:completion:@
writeAttributeLampAlarmModeWithValue_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampAlarmModeWithValue_completion mtrBaseClusterBallastConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampAlarmModeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampAlarmModeWithValue:params:completion:@
writeAttributeLampAlarmModeWithValue_params_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampAlarmModeWithValue_params_completion mtrBaseClusterBallastConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampAlarmModeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLampAlarmModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampAlarmModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampAlarmModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampAlarmModeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampAlarmModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampAlarmModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampAlarmModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampAlarmModeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLampBurnHoursTripPointWithCompletion:@
readAttributeLampBurnHoursTripPointWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampBurnHoursTripPointWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampBurnHoursTripPointWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampBurnHoursTripPointWithValue:completion:@
writeAttributeLampBurnHoursTripPointWithValue_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampBurnHoursTripPointWithValue_completion mtrBaseClusterBallastConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursTripPointWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLampBurnHoursTripPointWithValue:params:completion:@
writeAttributeLampBurnHoursTripPointWithValue_params_completion :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampBurnHoursTripPointWithValue_params_completion mtrBaseClusterBallastConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursTripPointWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLampBurnHoursTripPointWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampBurnHoursTripPointWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampBurnHoursTripPointWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampBurnHoursTripPointWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampBurnHoursTripPointWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampBurnHoursTripPointWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampBurnHoursTripPointWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampBurnHoursTripPointWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterBallastConfiguration  completion =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> IO (Id MTRBaseClusterBallastConfiguration)
init_ mtrBaseClusterBallastConfiguration  =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterBallastConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterBallastConfiguration -> device -> CUShort -> queue -> IO (Id MTRBaseClusterBallastConfiguration)
initWithDevice_endpoint_queue mtrBaseClusterBallastConfiguration  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributePhysicalMinLevelWithCompletionHandler:@
readAttributePhysicalMinLevelWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributePhysicalMinLevelWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributePhysicalMinLevelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePhysicalMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePhysicalMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePhysicalMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributePhysicalMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePhysicalMinLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePhysicalMinLevelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePhysicalMinLevelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePhysicalMinLevelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePhysicalMaxLevelWithCompletionHandler:@
readAttributePhysicalMaxLevelWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributePhysicalMaxLevelWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributePhysicalMaxLevelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePhysicalMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePhysicalMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePhysicalMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributePhysicalMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePhysicalMaxLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePhysicalMaxLevelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePhysicalMaxLevelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePhysicalMaxLevelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBallastStatusWithCompletionHandler:@
readAttributeBallastStatusWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeBallastStatusWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeBallastStatusWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBallastStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBallastStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBallastStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeBallastStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBallastStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBallastStatusWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBallastStatusWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBallastStatusWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMinLevelWithCompletionHandler:@
readAttributeMinLevelWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeMinLevelWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeMinLevelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeMinLevelWithValue:completionHandler:@
writeAttributeMinLevelWithValue_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeMinLevelWithValue_completionHandler mtrBaseClusterBallastConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeMinLevelWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeMinLevelWithValue:params:completionHandler:@
writeAttributeMinLevelWithValue_params_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeMinLevelWithValue_params_completionHandler mtrBaseClusterBallastConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeMinLevelWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinLevelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMaxLevelWithCompletionHandler:@
readAttributeMaxLevelWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeMaxLevelWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeMaxLevelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeMaxLevelWithValue:completionHandler:@
writeAttributeMaxLevelWithValue_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeMaxLevelWithValue_completionHandler mtrBaseClusterBallastConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeMaxLevelWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeMaxLevelWithValue:params:completionHandler:@
writeAttributeMaxLevelWithValue_params_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeMaxLevelWithValue_params_completionHandler mtrBaseClusterBallastConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeMaxLevelWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxLevelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeIntrinsicBalanceFactorWithCompletionHandler:@
readAttributeIntrinsicBalanceFactorWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeIntrinsicBalanceFactorWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeIntrinsicBalanceFactorWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeIntrinsicBalanceFactorWithValue:completionHandler:@
writeAttributeIntrinsicBalanceFactorWithValue_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeIntrinsicBalanceFactorWithValue_completionHandler mtrBaseClusterBallastConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeIntrinsicBalanceFactorWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeIntrinsicBalanceFactorWithValue:params:completionHandler:@
writeAttributeIntrinsicBalanceFactorWithValue_params_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeIntrinsicBalanceFactorWithValue_params_completionHandler mtrBaseClusterBallastConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeIntrinsicBalanceFactorWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeIntrinsicBalanceFactorWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeIntrinsicBalanceFactorWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeIntrinsicBalanceFactorWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeIntrinsicBalanceFactorWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeIntrinsicBalanceFactorWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeIntrinsicBalanceFactorWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeIntrinsicBalanceFactorWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeIntrinsicBalanceFactorWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBallastFactorAdjustmentWithCompletionHandler:@
readAttributeBallastFactorAdjustmentWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeBallastFactorAdjustmentWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeBallastFactorAdjustmentWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBallastFactorAdjustmentWithValue:completionHandler:@
writeAttributeBallastFactorAdjustmentWithValue_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeBallastFactorAdjustmentWithValue_completionHandler mtrBaseClusterBallastConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeBallastFactorAdjustmentWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBallastFactorAdjustmentWithValue:params:completionHandler:@
writeAttributeBallastFactorAdjustmentWithValue_params_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeBallastFactorAdjustmentWithValue_params_completionHandler mtrBaseClusterBallastConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeBallastFactorAdjustmentWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBallastFactorAdjustmentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBallastFactorAdjustmentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBallastFactorAdjustmentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeBallastFactorAdjustmentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBallastFactorAdjustmentWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBallastFactorAdjustmentWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBallastFactorAdjustmentWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBallastFactorAdjustmentWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLampQuantityWithCompletionHandler:@
readAttributeLampQuantityWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampQuantityWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampQuantityWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLampQuantityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampQuantityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampQuantityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampQuantityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampQuantityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampQuantityWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampQuantityWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampQuantityWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLampTypeWithCompletionHandler:@
readAttributeLampTypeWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampTypeWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampTypeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampTypeWithValue:completionHandler:@
writeAttributeLampTypeWithValue_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSString value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampTypeWithValue_completionHandler mtrBaseClusterBallastConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampTypeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampTypeWithValue:params:completionHandler:@
writeAttributeLampTypeWithValue_params_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampTypeWithValue_params_completionHandler mtrBaseClusterBallastConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampTypeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLampTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampTypeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampTypeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampTypeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLampManufacturerWithCompletionHandler:@
readAttributeLampManufacturerWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampManufacturerWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampManufacturerWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampManufacturerWithValue:completionHandler:@
writeAttributeLampManufacturerWithValue_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSString value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampManufacturerWithValue_completionHandler mtrBaseClusterBallastConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampManufacturerWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampManufacturerWithValue:params:completionHandler:@
writeAttributeLampManufacturerWithValue_params_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampManufacturerWithValue_params_completionHandler mtrBaseClusterBallastConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampManufacturerWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLampManufacturerWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampManufacturerWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampManufacturerWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampManufacturerWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampManufacturerWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampManufacturerWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampManufacturerWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampManufacturerWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLampRatedHoursWithCompletionHandler:@
readAttributeLampRatedHoursWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampRatedHoursWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampRatedHoursWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampRatedHoursWithValue:completionHandler:@
writeAttributeLampRatedHoursWithValue_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampRatedHoursWithValue_completionHandler mtrBaseClusterBallastConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampRatedHoursWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampRatedHoursWithValue:params:completionHandler:@
writeAttributeLampRatedHoursWithValue_params_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampRatedHoursWithValue_params_completionHandler mtrBaseClusterBallastConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampRatedHoursWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLampRatedHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampRatedHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampRatedHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampRatedHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampRatedHoursWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampRatedHoursWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampRatedHoursWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampRatedHoursWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLampBurnHoursWithCompletionHandler:@
readAttributeLampBurnHoursWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampBurnHoursWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampBurnHoursWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampBurnHoursWithValue:completionHandler:@
writeAttributeLampBurnHoursWithValue_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampBurnHoursWithValue_completionHandler mtrBaseClusterBallastConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampBurnHoursWithValue:params:completionHandler:@
writeAttributeLampBurnHoursWithValue_params_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampBurnHoursWithValue_params_completionHandler mtrBaseClusterBallastConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLampBurnHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampBurnHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampBurnHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampBurnHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampBurnHoursWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampBurnHoursWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampBurnHoursWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampBurnHoursWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLampAlarmModeWithCompletionHandler:@
readAttributeLampAlarmModeWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampAlarmModeWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampAlarmModeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampAlarmModeWithValue:completionHandler:@
writeAttributeLampAlarmModeWithValue_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampAlarmModeWithValue_completionHandler mtrBaseClusterBallastConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampAlarmModeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampAlarmModeWithValue:params:completionHandler:@
writeAttributeLampAlarmModeWithValue_params_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampAlarmModeWithValue_params_completionHandler mtrBaseClusterBallastConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampAlarmModeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLampAlarmModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampAlarmModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampAlarmModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampAlarmModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampAlarmModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampAlarmModeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampAlarmModeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampAlarmModeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLampBurnHoursTripPointWithCompletionHandler:@
readAttributeLampBurnHoursTripPointWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeLampBurnHoursTripPointWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeLampBurnHoursTripPointWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampBurnHoursTripPointWithValue:completionHandler:@
writeAttributeLampBurnHoursTripPointWithValue_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value) => mtrBaseClusterBallastConfiguration -> value -> Ptr () -> IO ()
writeAttributeLampBurnHoursTripPointWithValue_completionHandler mtrBaseClusterBallastConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursTripPointWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLampBurnHoursTripPointWithValue:params:completionHandler:@
writeAttributeLampBurnHoursTripPointWithValue_params_completionHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBallastConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeLampBurnHoursTripPointWithValue_params_completionHandler mtrBaseClusterBallastConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBallastConfiguration (mkSelector "writeAttributeLampBurnHoursTripPointWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLampBurnHoursTripPointWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampBurnHoursTripPointWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLampBurnHoursTripPointWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeLampBurnHoursTripPointWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLampBurnHoursTripPointWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampBurnHoursTripPointWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLampBurnHoursTripPointWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLampBurnHoursTripPointWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration => mtrBaseClusterBallastConfiguration -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterBallastConfiguration  completionHandler =
    sendMsg mtrBaseClusterBallastConfiguration (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBallastConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBallastConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBallastConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterBallastConfiguration mtrBaseClusterBallastConfiguration, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterBallastConfiguration -> device -> endpointID -> queue -> IO (Id MTRBaseClusterBallastConfiguration)
initWithDevice_endpointID_queue mtrBaseClusterBallastConfiguration  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterBallastConfiguration (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributePhysicalMinLevelWithCompletion:@
readAttributePhysicalMinLevelWithCompletionSelector :: Selector
readAttributePhysicalMinLevelWithCompletionSelector = mkSelector "readAttributePhysicalMinLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributePhysicalMinLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePhysicalMinLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePhysicalMinLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePhysicalMinLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePhysicalMinLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributePhysicalMinLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePhysicalMinLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePhysicalMinLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePhysicalMaxLevelWithCompletion:@
readAttributePhysicalMaxLevelWithCompletionSelector :: Selector
readAttributePhysicalMaxLevelWithCompletionSelector = mkSelector "readAttributePhysicalMaxLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributePhysicalMaxLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePhysicalMaxLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePhysicalMaxLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePhysicalMaxLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePhysicalMaxLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributePhysicalMaxLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePhysicalMaxLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePhysicalMaxLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBallastStatusWithCompletion:@
readAttributeBallastStatusWithCompletionSelector :: Selector
readAttributeBallastStatusWithCompletionSelector = mkSelector "readAttributeBallastStatusWithCompletion:"

-- | @Selector@ for @subscribeAttributeBallastStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBallastStatusWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBallastStatusWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBallastStatusWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBallastStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeBallastStatusWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBallastStatusWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBallastStatusWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinLevelWithCompletion:@
readAttributeMinLevelWithCompletionSelector :: Selector
readAttributeMinLevelWithCompletionSelector = mkSelector "readAttributeMinLevelWithCompletion:"

-- | @Selector@ for @writeAttributeMinLevelWithValue:completion:@
writeAttributeMinLevelWithValue_completionSelector :: Selector
writeAttributeMinLevelWithValue_completionSelector = mkSelector "writeAttributeMinLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeMinLevelWithValue:params:completion:@
writeAttributeMinLevelWithValue_params_completionSelector :: Selector
writeAttributeMinLevelWithValue_params_completionSelector = mkSelector "writeAttributeMinLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeMinLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMinLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxLevelWithCompletion:@
readAttributeMaxLevelWithCompletionSelector :: Selector
readAttributeMaxLevelWithCompletionSelector = mkSelector "readAttributeMaxLevelWithCompletion:"

-- | @Selector@ for @writeAttributeMaxLevelWithValue:completion:@
writeAttributeMaxLevelWithValue_completionSelector :: Selector
writeAttributeMaxLevelWithValue_completionSelector = mkSelector "writeAttributeMaxLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeMaxLevelWithValue:params:completion:@
writeAttributeMaxLevelWithValue_params_completionSelector :: Selector
writeAttributeMaxLevelWithValue_params_completionSelector = mkSelector "writeAttributeMaxLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeMaxLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeIntrinsicBallastFactorWithCompletion:@
readAttributeIntrinsicBallastFactorWithCompletionSelector :: Selector
readAttributeIntrinsicBallastFactorWithCompletionSelector = mkSelector "readAttributeIntrinsicBallastFactorWithCompletion:"

-- | @Selector@ for @writeAttributeIntrinsicBallastFactorWithValue:completion:@
writeAttributeIntrinsicBallastFactorWithValue_completionSelector :: Selector
writeAttributeIntrinsicBallastFactorWithValue_completionSelector = mkSelector "writeAttributeIntrinsicBallastFactorWithValue:completion:"

-- | @Selector@ for @writeAttributeIntrinsicBallastFactorWithValue:params:completion:@
writeAttributeIntrinsicBallastFactorWithValue_params_completionSelector :: Selector
writeAttributeIntrinsicBallastFactorWithValue_params_completionSelector = mkSelector "writeAttributeIntrinsicBallastFactorWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeIntrinsicBallastFactorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIntrinsicBallastFactorWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeIntrinsicBallastFactorWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeIntrinsicBallastFactorWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeIntrinsicBallastFactorWithClusterStateCache:endpoint:queue:completion:@
readAttributeIntrinsicBallastFactorWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeIntrinsicBallastFactorWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeIntrinsicBallastFactorWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBallastFactorAdjustmentWithCompletion:@
readAttributeBallastFactorAdjustmentWithCompletionSelector :: Selector
readAttributeBallastFactorAdjustmentWithCompletionSelector = mkSelector "readAttributeBallastFactorAdjustmentWithCompletion:"

-- | @Selector@ for @writeAttributeBallastFactorAdjustmentWithValue:completion:@
writeAttributeBallastFactorAdjustmentWithValue_completionSelector :: Selector
writeAttributeBallastFactorAdjustmentWithValue_completionSelector = mkSelector "writeAttributeBallastFactorAdjustmentWithValue:completion:"

-- | @Selector@ for @writeAttributeBallastFactorAdjustmentWithValue:params:completion:@
writeAttributeBallastFactorAdjustmentWithValue_params_completionSelector :: Selector
writeAttributeBallastFactorAdjustmentWithValue_params_completionSelector = mkSelector "writeAttributeBallastFactorAdjustmentWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeBallastFactorAdjustmentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBallastFactorAdjustmentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBallastFactorAdjustmentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBallastFactorAdjustmentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBallastFactorAdjustmentWithClusterStateCache:endpoint:queue:completion:@
readAttributeBallastFactorAdjustmentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBallastFactorAdjustmentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBallastFactorAdjustmentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLampQuantityWithCompletion:@
readAttributeLampQuantityWithCompletionSelector :: Selector
readAttributeLampQuantityWithCompletionSelector = mkSelector "readAttributeLampQuantityWithCompletion:"

-- | @Selector@ for @subscribeAttributeLampQuantityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampQuantityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampQuantityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampQuantityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampQuantityWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampQuantityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLampQuantityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLampQuantityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLampTypeWithCompletion:@
readAttributeLampTypeWithCompletionSelector :: Selector
readAttributeLampTypeWithCompletionSelector = mkSelector "readAttributeLampTypeWithCompletion:"

-- | @Selector@ for @writeAttributeLampTypeWithValue:completion:@
writeAttributeLampTypeWithValue_completionSelector :: Selector
writeAttributeLampTypeWithValue_completionSelector = mkSelector "writeAttributeLampTypeWithValue:completion:"

-- | @Selector@ for @writeAttributeLampTypeWithValue:params:completion:@
writeAttributeLampTypeWithValue_params_completionSelector :: Selector
writeAttributeLampTypeWithValue_params_completionSelector = mkSelector "writeAttributeLampTypeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLampTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLampTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLampTypeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLampManufacturerWithCompletion:@
readAttributeLampManufacturerWithCompletionSelector :: Selector
readAttributeLampManufacturerWithCompletionSelector = mkSelector "readAttributeLampManufacturerWithCompletion:"

-- | @Selector@ for @writeAttributeLampManufacturerWithValue:completion:@
writeAttributeLampManufacturerWithValue_completionSelector :: Selector
writeAttributeLampManufacturerWithValue_completionSelector = mkSelector "writeAttributeLampManufacturerWithValue:completion:"

-- | @Selector@ for @writeAttributeLampManufacturerWithValue:params:completion:@
writeAttributeLampManufacturerWithValue_params_completionSelector :: Selector
writeAttributeLampManufacturerWithValue_params_completionSelector = mkSelector "writeAttributeLampManufacturerWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLampManufacturerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampManufacturerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampManufacturerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampManufacturerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampManufacturerWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampManufacturerWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLampManufacturerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLampManufacturerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLampRatedHoursWithCompletion:@
readAttributeLampRatedHoursWithCompletionSelector :: Selector
readAttributeLampRatedHoursWithCompletionSelector = mkSelector "readAttributeLampRatedHoursWithCompletion:"

-- | @Selector@ for @writeAttributeLampRatedHoursWithValue:completion:@
writeAttributeLampRatedHoursWithValue_completionSelector :: Selector
writeAttributeLampRatedHoursWithValue_completionSelector = mkSelector "writeAttributeLampRatedHoursWithValue:completion:"

-- | @Selector@ for @writeAttributeLampRatedHoursWithValue:params:completion:@
writeAttributeLampRatedHoursWithValue_params_completionSelector :: Selector
writeAttributeLampRatedHoursWithValue_params_completionSelector = mkSelector "writeAttributeLampRatedHoursWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLampRatedHoursWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampRatedHoursWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampRatedHoursWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampRatedHoursWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampRatedHoursWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampRatedHoursWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLampRatedHoursWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLampRatedHoursWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLampBurnHoursWithCompletion:@
readAttributeLampBurnHoursWithCompletionSelector :: Selector
readAttributeLampBurnHoursWithCompletionSelector = mkSelector "readAttributeLampBurnHoursWithCompletion:"

-- | @Selector@ for @writeAttributeLampBurnHoursWithValue:completion:@
writeAttributeLampBurnHoursWithValue_completionSelector :: Selector
writeAttributeLampBurnHoursWithValue_completionSelector = mkSelector "writeAttributeLampBurnHoursWithValue:completion:"

-- | @Selector@ for @writeAttributeLampBurnHoursWithValue:params:completion:@
writeAttributeLampBurnHoursWithValue_params_completionSelector :: Selector
writeAttributeLampBurnHoursWithValue_params_completionSelector = mkSelector "writeAttributeLampBurnHoursWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLampBurnHoursWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampBurnHoursWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampBurnHoursWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampBurnHoursWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampBurnHoursWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampBurnHoursWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLampBurnHoursWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLampBurnHoursWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLampAlarmModeWithCompletion:@
readAttributeLampAlarmModeWithCompletionSelector :: Selector
readAttributeLampAlarmModeWithCompletionSelector = mkSelector "readAttributeLampAlarmModeWithCompletion:"

-- | @Selector@ for @writeAttributeLampAlarmModeWithValue:completion:@
writeAttributeLampAlarmModeWithValue_completionSelector :: Selector
writeAttributeLampAlarmModeWithValue_completionSelector = mkSelector "writeAttributeLampAlarmModeWithValue:completion:"

-- | @Selector@ for @writeAttributeLampAlarmModeWithValue:params:completion:@
writeAttributeLampAlarmModeWithValue_params_completionSelector :: Selector
writeAttributeLampAlarmModeWithValue_params_completionSelector = mkSelector "writeAttributeLampAlarmModeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLampAlarmModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampAlarmModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampAlarmModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampAlarmModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampAlarmModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampAlarmModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLampAlarmModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLampAlarmModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLampBurnHoursTripPointWithCompletion:@
readAttributeLampBurnHoursTripPointWithCompletionSelector :: Selector
readAttributeLampBurnHoursTripPointWithCompletionSelector = mkSelector "readAttributeLampBurnHoursTripPointWithCompletion:"

-- | @Selector@ for @writeAttributeLampBurnHoursTripPointWithValue:completion:@
writeAttributeLampBurnHoursTripPointWithValue_completionSelector :: Selector
writeAttributeLampBurnHoursTripPointWithValue_completionSelector = mkSelector "writeAttributeLampBurnHoursTripPointWithValue:completion:"

-- | @Selector@ for @writeAttributeLampBurnHoursTripPointWithValue:params:completion:@
writeAttributeLampBurnHoursTripPointWithValue_params_completionSelector :: Selector
writeAttributeLampBurnHoursTripPointWithValue_params_completionSelector = mkSelector "writeAttributeLampBurnHoursTripPointWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLampBurnHoursTripPointWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLampBurnHoursTripPointWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampBurnHoursTripPointWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampBurnHoursTripPointWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampBurnHoursTripPointWithClusterStateCache:endpoint:queue:completion:@
readAttributeLampBurnHoursTripPointWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLampBurnHoursTripPointWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLampBurnHoursTripPointWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @readAttributePhysicalMinLevelWithCompletionHandler:@
readAttributePhysicalMinLevelWithCompletionHandlerSelector :: Selector
readAttributePhysicalMinLevelWithCompletionHandlerSelector = mkSelector "readAttributePhysicalMinLevelWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePhysicalMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePhysicalMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePhysicalMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePhysicalMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePhysicalMinLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePhysicalMinLevelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePhysicalMinLevelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePhysicalMinLevelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePhysicalMaxLevelWithCompletionHandler:@
readAttributePhysicalMaxLevelWithCompletionHandlerSelector :: Selector
readAttributePhysicalMaxLevelWithCompletionHandlerSelector = mkSelector "readAttributePhysicalMaxLevelWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePhysicalMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePhysicalMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePhysicalMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePhysicalMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePhysicalMaxLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePhysicalMaxLevelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePhysicalMaxLevelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePhysicalMaxLevelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBallastStatusWithCompletionHandler:@
readAttributeBallastStatusWithCompletionHandlerSelector :: Selector
readAttributeBallastStatusWithCompletionHandlerSelector = mkSelector "readAttributeBallastStatusWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeBallastStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBallastStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBallastStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBallastStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBallastStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBallastStatusWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBallastStatusWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBallastStatusWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMinLevelWithCompletionHandler:@
readAttributeMinLevelWithCompletionHandlerSelector :: Selector
readAttributeMinLevelWithCompletionHandlerSelector = mkSelector "readAttributeMinLevelWithCompletionHandler:"

-- | @Selector@ for @writeAttributeMinLevelWithValue:completionHandler:@
writeAttributeMinLevelWithValue_completionHandlerSelector :: Selector
writeAttributeMinLevelWithValue_completionHandlerSelector = mkSelector "writeAttributeMinLevelWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeMinLevelWithValue:params:completionHandler:@
writeAttributeMinLevelWithValue_params_completionHandlerSelector :: Selector
writeAttributeMinLevelWithValue_params_completionHandlerSelector = mkSelector "writeAttributeMinLevelWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMinLevelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMinLevelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMaxLevelWithCompletionHandler:@
readAttributeMaxLevelWithCompletionHandlerSelector :: Selector
readAttributeMaxLevelWithCompletionHandlerSelector = mkSelector "readAttributeMaxLevelWithCompletionHandler:"

-- | @Selector@ for @writeAttributeMaxLevelWithValue:completionHandler:@
writeAttributeMaxLevelWithValue_completionHandlerSelector :: Selector
writeAttributeMaxLevelWithValue_completionHandlerSelector = mkSelector "writeAttributeMaxLevelWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeMaxLevelWithValue:params:completionHandler:@
writeAttributeMaxLevelWithValue_params_completionHandlerSelector :: Selector
writeAttributeMaxLevelWithValue_params_completionHandlerSelector = mkSelector "writeAttributeMaxLevelWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxLevelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxLevelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxLevelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMaxLevelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMaxLevelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeIntrinsicBalanceFactorWithCompletionHandler:@
readAttributeIntrinsicBalanceFactorWithCompletionHandlerSelector :: Selector
readAttributeIntrinsicBalanceFactorWithCompletionHandlerSelector = mkSelector "readAttributeIntrinsicBalanceFactorWithCompletionHandler:"

-- | @Selector@ for @writeAttributeIntrinsicBalanceFactorWithValue:completionHandler:@
writeAttributeIntrinsicBalanceFactorWithValue_completionHandlerSelector :: Selector
writeAttributeIntrinsicBalanceFactorWithValue_completionHandlerSelector = mkSelector "writeAttributeIntrinsicBalanceFactorWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeIntrinsicBalanceFactorWithValue:params:completionHandler:@
writeAttributeIntrinsicBalanceFactorWithValue_params_completionHandlerSelector :: Selector
writeAttributeIntrinsicBalanceFactorWithValue_params_completionHandlerSelector = mkSelector "writeAttributeIntrinsicBalanceFactorWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeIntrinsicBalanceFactorWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeIntrinsicBalanceFactorWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeIntrinsicBalanceFactorWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeIntrinsicBalanceFactorWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeIntrinsicBalanceFactorWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeIntrinsicBalanceFactorWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeIntrinsicBalanceFactorWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeIntrinsicBalanceFactorWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBallastFactorAdjustmentWithCompletionHandler:@
readAttributeBallastFactorAdjustmentWithCompletionHandlerSelector :: Selector
readAttributeBallastFactorAdjustmentWithCompletionHandlerSelector = mkSelector "readAttributeBallastFactorAdjustmentWithCompletionHandler:"

-- | @Selector@ for @writeAttributeBallastFactorAdjustmentWithValue:completionHandler:@
writeAttributeBallastFactorAdjustmentWithValue_completionHandlerSelector :: Selector
writeAttributeBallastFactorAdjustmentWithValue_completionHandlerSelector = mkSelector "writeAttributeBallastFactorAdjustmentWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeBallastFactorAdjustmentWithValue:params:completionHandler:@
writeAttributeBallastFactorAdjustmentWithValue_params_completionHandlerSelector :: Selector
writeAttributeBallastFactorAdjustmentWithValue_params_completionHandlerSelector = mkSelector "writeAttributeBallastFactorAdjustmentWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeBallastFactorAdjustmentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBallastFactorAdjustmentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBallastFactorAdjustmentWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBallastFactorAdjustmentWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBallastFactorAdjustmentWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBallastFactorAdjustmentWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBallastFactorAdjustmentWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBallastFactorAdjustmentWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLampQuantityWithCompletionHandler:@
readAttributeLampQuantityWithCompletionHandlerSelector :: Selector
readAttributeLampQuantityWithCompletionHandlerSelector = mkSelector "readAttributeLampQuantityWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeLampQuantityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampQuantityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampQuantityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampQuantityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampQuantityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampQuantityWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLampQuantityWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLampQuantityWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLampTypeWithCompletionHandler:@
readAttributeLampTypeWithCompletionHandlerSelector :: Selector
readAttributeLampTypeWithCompletionHandlerSelector = mkSelector "readAttributeLampTypeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeLampTypeWithValue:completionHandler:@
writeAttributeLampTypeWithValue_completionHandlerSelector :: Selector
writeAttributeLampTypeWithValue_completionHandlerSelector = mkSelector "writeAttributeLampTypeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeLampTypeWithValue:params:completionHandler:@
writeAttributeLampTypeWithValue_params_completionHandlerSelector :: Selector
writeAttributeLampTypeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeLampTypeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeLampTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampTypeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLampTypeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLampTypeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLampManufacturerWithCompletionHandler:@
readAttributeLampManufacturerWithCompletionHandlerSelector :: Selector
readAttributeLampManufacturerWithCompletionHandlerSelector = mkSelector "readAttributeLampManufacturerWithCompletionHandler:"

-- | @Selector@ for @writeAttributeLampManufacturerWithValue:completionHandler:@
writeAttributeLampManufacturerWithValue_completionHandlerSelector :: Selector
writeAttributeLampManufacturerWithValue_completionHandlerSelector = mkSelector "writeAttributeLampManufacturerWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeLampManufacturerWithValue:params:completionHandler:@
writeAttributeLampManufacturerWithValue_params_completionHandlerSelector :: Selector
writeAttributeLampManufacturerWithValue_params_completionHandlerSelector = mkSelector "writeAttributeLampManufacturerWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeLampManufacturerWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampManufacturerWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampManufacturerWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampManufacturerWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampManufacturerWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampManufacturerWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLampManufacturerWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLampManufacturerWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLampRatedHoursWithCompletionHandler:@
readAttributeLampRatedHoursWithCompletionHandlerSelector :: Selector
readAttributeLampRatedHoursWithCompletionHandlerSelector = mkSelector "readAttributeLampRatedHoursWithCompletionHandler:"

-- | @Selector@ for @writeAttributeLampRatedHoursWithValue:completionHandler:@
writeAttributeLampRatedHoursWithValue_completionHandlerSelector :: Selector
writeAttributeLampRatedHoursWithValue_completionHandlerSelector = mkSelector "writeAttributeLampRatedHoursWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeLampRatedHoursWithValue:params:completionHandler:@
writeAttributeLampRatedHoursWithValue_params_completionHandlerSelector :: Selector
writeAttributeLampRatedHoursWithValue_params_completionHandlerSelector = mkSelector "writeAttributeLampRatedHoursWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeLampRatedHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampRatedHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampRatedHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampRatedHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampRatedHoursWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampRatedHoursWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLampRatedHoursWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLampRatedHoursWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLampBurnHoursWithCompletionHandler:@
readAttributeLampBurnHoursWithCompletionHandlerSelector :: Selector
readAttributeLampBurnHoursWithCompletionHandlerSelector = mkSelector "readAttributeLampBurnHoursWithCompletionHandler:"

-- | @Selector@ for @writeAttributeLampBurnHoursWithValue:completionHandler:@
writeAttributeLampBurnHoursWithValue_completionHandlerSelector :: Selector
writeAttributeLampBurnHoursWithValue_completionHandlerSelector = mkSelector "writeAttributeLampBurnHoursWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeLampBurnHoursWithValue:params:completionHandler:@
writeAttributeLampBurnHoursWithValue_params_completionHandlerSelector :: Selector
writeAttributeLampBurnHoursWithValue_params_completionHandlerSelector = mkSelector "writeAttributeLampBurnHoursWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeLampBurnHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampBurnHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampBurnHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampBurnHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampBurnHoursWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampBurnHoursWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLampBurnHoursWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLampBurnHoursWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLampAlarmModeWithCompletionHandler:@
readAttributeLampAlarmModeWithCompletionHandlerSelector :: Selector
readAttributeLampAlarmModeWithCompletionHandlerSelector = mkSelector "readAttributeLampAlarmModeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeLampAlarmModeWithValue:completionHandler:@
writeAttributeLampAlarmModeWithValue_completionHandlerSelector :: Selector
writeAttributeLampAlarmModeWithValue_completionHandlerSelector = mkSelector "writeAttributeLampAlarmModeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeLampAlarmModeWithValue:params:completionHandler:@
writeAttributeLampAlarmModeWithValue_params_completionHandlerSelector :: Selector
writeAttributeLampAlarmModeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeLampAlarmModeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeLampAlarmModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampAlarmModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampAlarmModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampAlarmModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampAlarmModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampAlarmModeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLampAlarmModeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLampAlarmModeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLampBurnHoursTripPointWithCompletionHandler:@
readAttributeLampBurnHoursTripPointWithCompletionHandlerSelector :: Selector
readAttributeLampBurnHoursTripPointWithCompletionHandlerSelector = mkSelector "readAttributeLampBurnHoursTripPointWithCompletionHandler:"

-- | @Selector@ for @writeAttributeLampBurnHoursTripPointWithValue:completionHandler:@
writeAttributeLampBurnHoursTripPointWithValue_completionHandlerSelector :: Selector
writeAttributeLampBurnHoursTripPointWithValue_completionHandlerSelector = mkSelector "writeAttributeLampBurnHoursTripPointWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeLampBurnHoursTripPointWithValue:params:completionHandler:@
writeAttributeLampBurnHoursTripPointWithValue_params_completionHandlerSelector :: Selector
writeAttributeLampBurnHoursTripPointWithValue_params_completionHandlerSelector = mkSelector "writeAttributeLampBurnHoursTripPointWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeLampBurnHoursTripPointWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLampBurnHoursTripPointWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLampBurnHoursTripPointWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLampBurnHoursTripPointWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLampBurnHoursTripPointWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLampBurnHoursTripPointWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLampBurnHoursTripPointWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLampBurnHoursTripPointWithAttributeCache:endpoint:queue:completionHandler:"

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

