{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Tariff
--
-- The CommodityTariffCluster provides the mechanism for communicating Commodity Tariff information within the premises.
--
-- Generated bindings for @MTRBaseClusterCommodityTariff@.
module ObjC.Matter.MTRBaseClusterCommodityTariff
  ( MTRBaseClusterCommodityTariff
  , IsMTRBaseClusterCommodityTariff(..)
  , getTariffComponentWithParams_completion
  , getDayEntryWithParams_completion
  , readAttributeTariffInfoWithCompletion
  , subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completion
  , readAttributeTariffUnitWithCompletion
  , subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion
  , readAttributeStartDateWithCompletion
  , subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandler
  , readAttributeStartDateWithClusterStateCache_endpoint_queue_completion
  , readAttributeDayEntriesWithCompletion
  , subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandler
  , readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completion
  , readAttributeDayPatternsWithCompletion
  , subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandler
  , readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completion
  , readAttributeCalendarPeriodsWithCompletion
  , subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completion
  , readAttributeIndividualDaysWithCompletion
  , subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandler
  , readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentDayWithCompletion
  , subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextDayWithCompletion
  , subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextDayWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentDayEntryWithCompletion
  , subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentDayEntryDateWithCompletion
  , subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextDayEntryWithCompletion
  , subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextDayEntryDateWithCompletion
  , subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completion
  , readAttributeTariffComponentsWithCompletion
  , subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completion
  , readAttributeTariffPeriodsWithCompletion
  , subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentTariffComponentsWithCompletion
  , subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextTariffComponentsWithCompletion
  , subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultRandomizationOffsetWithCompletion
  , subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultRandomizationTypeWithCompletion
  , subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completion
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
  , getTariffComponentWithParams_completionSelector
  , getDayEntryWithParams_completionSelector
  , readAttributeTariffInfoWithCompletionSelector
  , subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTariffUnitWithCompletionSelector
  , subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStartDateWithCompletionSelector
  , subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStartDateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDayEntriesWithCompletionSelector
  , subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDayPatternsWithCompletionSelector
  , subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCalendarPeriodsWithCompletionSelector
  , subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeIndividualDaysWithCompletionSelector
  , subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentDayWithCompletionSelector
  , subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextDayWithCompletionSelector
  , subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNextDayWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentDayEntryWithCompletionSelector
  , subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentDayEntryDateWithCompletionSelector
  , subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextDayEntryWithCompletionSelector
  , subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextDayEntryDateWithCompletionSelector
  , subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTariffComponentsWithCompletionSelector
  , subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTariffPeriodsWithCompletionSelector
  , subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentTariffComponentsWithCompletionSelector
  , subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextTariffComponentsWithCompletionSelector
  , subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultRandomizationOffsetWithCompletionSelector
  , subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultRandomizationTypeWithCompletionSelector
  , subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command GetTariffComponent
--
-- The GetTariffComponent command allows a client to request information for a tariff component identifier that may no longer be available in the TariffPeriods attributes.
--
-- ObjC selector: @- getTariffComponentWithParams:completion:@
getTariffComponentWithParams_completion :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRCommodityTariffClusterGetTariffComponentParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> IO ()
getTariffComponentWithParams_completion mtrBaseClusterCommodityTariff  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "getTariffComponentWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command GetDayEntry
--
-- The GetDayEntry command allows a client to request information for a calendar day entry identifier that may no longer be available in the CalendarPeriods or IndividualDays attributes.
--
-- ObjC selector: @- getDayEntryWithParams:completion:@
getDayEntryWithParams_completion :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRCommodityTariffClusterGetDayEntryParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> IO ()
getDayEntryWithParams_completion mtrBaseClusterCommodityTariff  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "getDayEntryWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTariffInfoWithCompletion:@
readAttributeTariffInfoWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeTariffInfoWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeTariffInfoWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTariffInfoWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeTariffInfoWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTariffInfoWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTariffInfoWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeTariffUnitWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeTariffUnitWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStartDateWithCompletion:@
readAttributeStartDateWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeStartDateWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeStartDateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStartDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeStartDateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStartDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStartDateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDayEntriesWithCompletion:@
readAttributeDayEntriesWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeDayEntriesWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeDayEntriesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDayEntriesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeDayEntriesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDayEntriesWithClusterStateCache:endpoint:queue:completion:@
readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDayEntriesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDayPatternsWithCompletion:@
readAttributeDayPatternsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeDayPatternsWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeDayPatternsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDayPatternsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeDayPatternsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDayPatternsWithClusterStateCache:endpoint:queue:completion:@
readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDayPatternsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCalendarPeriodsWithCompletion:@
readAttributeCalendarPeriodsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeCalendarPeriodsWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeCalendarPeriodsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCalendarPeriodsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeCalendarPeriodsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCalendarPeriodsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCalendarPeriodsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeIndividualDaysWithCompletion:@
readAttributeIndividualDaysWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeIndividualDaysWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeIndividualDaysWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeIndividualDaysWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeIndividualDaysWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeIndividualDaysWithClusterStateCache:endpoint:queue:completion:@
readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeIndividualDaysWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentDayWithCompletion:@
readAttributeCurrentDayWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeCurrentDayWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeCurrentDayWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentDayWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeCurrentDayWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentDayWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentDayWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNextDayWithCompletion:@
readAttributeNextDayWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeNextDayWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeNextDayWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNextDayWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeNextDayWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNextDayWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextDayWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNextDayWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentDayEntryWithCompletion:@
readAttributeCurrentDayEntryWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeCurrentDayEntryWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeCurrentDayEntryWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentDayEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeCurrentDayEntryWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentDayEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentDayEntryWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentDayEntryDateWithCompletion:@
readAttributeCurrentDayEntryDateWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeCurrentDayEntryDateWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeCurrentDayEntryDateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentDayEntryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeCurrentDayEntryDateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentDayEntryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentDayEntryDateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNextDayEntryWithCompletion:@
readAttributeNextDayEntryWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeNextDayEntryWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeNextDayEntryWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNextDayEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeNextDayEntryWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNextDayEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNextDayEntryWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNextDayEntryDateWithCompletion:@
readAttributeNextDayEntryDateWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeNextDayEntryDateWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeNextDayEntryDateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNextDayEntryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeNextDayEntryDateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNextDayEntryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNextDayEntryDateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTariffComponentsWithCompletion:@
readAttributeTariffComponentsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeTariffComponentsWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeTariffComponentsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeTariffComponentsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTariffComponentsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTariffPeriodsWithCompletion:@
readAttributeTariffPeriodsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeTariffPeriodsWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeTariffPeriodsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTariffPeriodsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeTariffPeriodsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTariffPeriodsWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTariffPeriodsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentTariffComponentsWithCompletion:@
readAttributeCurrentTariffComponentsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeCurrentTariffComponentsWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeCurrentTariffComponentsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeCurrentTariffComponentsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentTariffComponentsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNextTariffComponentsWithCompletion:@
readAttributeNextTariffComponentsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeNextTariffComponentsWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeNextTariffComponentsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNextTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeNextTariffComponentsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNextTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNextTariffComponentsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDefaultRandomizationOffsetWithCompletion:@
readAttributeDefaultRandomizationOffsetWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeDefaultRandomizationOffsetWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeDefaultRandomizationOffsetWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDefaultRandomizationOffsetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeDefaultRandomizationOffsetWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDefaultRandomizationOffsetWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDefaultRandomizationOffsetWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDefaultRandomizationTypeWithCompletion:@
readAttributeDefaultRandomizationTypeWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeDefaultRandomizationTypeWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeDefaultRandomizationTypeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDefaultRandomizationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeDefaultRandomizationTypeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDefaultRandomizationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDefaultRandomizationTypeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterCommodityTariff  completion =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityTariff (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> IO (Id MTRBaseClusterCommodityTariff)
init_ mtrBaseClusterCommodityTariff  =
    sendMsg mtrBaseClusterCommodityTariff (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterCommodityTariff)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterCommodityTariff -> device -> endpointID -> queue -> IO (Id MTRBaseClusterCommodityTariff)
initWithDevice_endpointID_queue mtrBaseClusterCommodityTariff  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterCommodityTariff (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getTariffComponentWithParams:completion:@
getTariffComponentWithParams_completionSelector :: Selector
getTariffComponentWithParams_completionSelector = mkSelector "getTariffComponentWithParams:completion:"

-- | @Selector@ for @getDayEntryWithParams:completion:@
getDayEntryWithParams_completionSelector :: Selector
getDayEntryWithParams_completionSelector = mkSelector "getDayEntryWithParams:completion:"

-- | @Selector@ for @readAttributeTariffInfoWithCompletion:@
readAttributeTariffInfoWithCompletionSelector :: Selector
readAttributeTariffInfoWithCompletionSelector = mkSelector "readAttributeTariffInfoWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffInfoWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffInfoWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffInfoWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffInfoWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletionSelector :: Selector
readAttributeTariffUnitWithCompletionSelector = mkSelector "readAttributeTariffUnitWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStartDateWithCompletion:@
readAttributeStartDateWithCompletionSelector :: Selector
readAttributeStartDateWithCompletionSelector = mkSelector "readAttributeStartDateWithCompletion:"

-- | @Selector@ for @subscribeAttributeStartDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartDateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartDateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStartDateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStartDateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDayEntriesWithCompletion:@
readAttributeDayEntriesWithCompletionSelector :: Selector
readAttributeDayEntriesWithCompletionSelector = mkSelector "readAttributeDayEntriesWithCompletion:"

-- | @Selector@ for @subscribeAttributeDayEntriesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDayEntriesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDayEntriesWithClusterStateCache:endpoint:queue:completion:@
readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDayEntriesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDayPatternsWithCompletion:@
readAttributeDayPatternsWithCompletionSelector :: Selector
readAttributeDayPatternsWithCompletionSelector = mkSelector "readAttributeDayPatternsWithCompletion:"

-- | @Selector@ for @subscribeAttributeDayPatternsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDayPatternsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDayPatternsWithClusterStateCache:endpoint:queue:completion:@
readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDayPatternsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCalendarPeriodsWithCompletion:@
readAttributeCalendarPeriodsWithCompletionSelector :: Selector
readAttributeCalendarPeriodsWithCompletionSelector = mkSelector "readAttributeCalendarPeriodsWithCompletion:"

-- | @Selector@ for @subscribeAttributeCalendarPeriodsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCalendarPeriodsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCalendarPeriodsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCalendarPeriodsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeIndividualDaysWithCompletion:@
readAttributeIndividualDaysWithCompletionSelector :: Selector
readAttributeIndividualDaysWithCompletionSelector = mkSelector "readAttributeIndividualDaysWithCompletion:"

-- | @Selector@ for @subscribeAttributeIndividualDaysWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeIndividualDaysWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeIndividualDaysWithClusterStateCache:endpoint:queue:completion:@
readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeIndividualDaysWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentDayWithCompletion:@
readAttributeCurrentDayWithCompletionSelector :: Selector
readAttributeCurrentDayWithCompletionSelector = mkSelector "readAttributeCurrentDayWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentDayWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentDayWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentDayWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentDayWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextDayWithCompletion:@
readAttributeNextDayWithCompletionSelector :: Selector
readAttributeNextDayWithCompletionSelector = mkSelector "readAttributeNextDayWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextDayWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextDayWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextDayWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNextDayWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextDayWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentDayEntryWithCompletion:@
readAttributeCurrentDayEntryWithCompletionSelector :: Selector
readAttributeCurrentDayEntryWithCompletionSelector = mkSelector "readAttributeCurrentDayEntryWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentDayEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentDayEntryWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentDayEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentDayEntryWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentDayEntryDateWithCompletion:@
readAttributeCurrentDayEntryDateWithCompletionSelector :: Selector
readAttributeCurrentDayEntryDateWithCompletionSelector = mkSelector "readAttributeCurrentDayEntryDateWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentDayEntryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentDayEntryDateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentDayEntryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentDayEntryDateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextDayEntryWithCompletion:@
readAttributeNextDayEntryWithCompletionSelector :: Selector
readAttributeNextDayEntryWithCompletionSelector = mkSelector "readAttributeNextDayEntryWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextDayEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextDayEntryWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextDayEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextDayEntryWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextDayEntryDateWithCompletion:@
readAttributeNextDayEntryDateWithCompletionSelector :: Selector
readAttributeNextDayEntryDateWithCompletionSelector = mkSelector "readAttributeNextDayEntryDateWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextDayEntryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextDayEntryDateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextDayEntryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextDayEntryDateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTariffComponentsWithCompletion:@
readAttributeTariffComponentsWithCompletionSelector :: Selector
readAttributeTariffComponentsWithCompletionSelector = mkSelector "readAttributeTariffComponentsWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffComponentsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffComponentsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTariffPeriodsWithCompletion:@
readAttributeTariffPeriodsWithCompletionSelector :: Selector
readAttributeTariffPeriodsWithCompletionSelector = mkSelector "readAttributeTariffPeriodsWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffPeriodsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffPeriodsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffPeriodsWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffPeriodsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentTariffComponentsWithCompletion:@
readAttributeCurrentTariffComponentsWithCompletionSelector :: Selector
readAttributeCurrentTariffComponentsWithCompletionSelector = mkSelector "readAttributeCurrentTariffComponentsWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentTariffComponentsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentTariffComponentsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextTariffComponentsWithCompletion:@
readAttributeNextTariffComponentsWithCompletionSelector :: Selector
readAttributeNextTariffComponentsWithCompletionSelector = mkSelector "readAttributeNextTariffComponentsWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextTariffComponentsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextTariffComponentsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultRandomizationOffsetWithCompletion:@
readAttributeDefaultRandomizationOffsetWithCompletionSelector :: Selector
readAttributeDefaultRandomizationOffsetWithCompletionSelector = mkSelector "readAttributeDefaultRandomizationOffsetWithCompletion:"

-- | @Selector@ for @subscribeAttributeDefaultRandomizationOffsetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultRandomizationOffsetWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultRandomizationOffsetWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultRandomizationOffsetWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultRandomizationTypeWithCompletion:@
readAttributeDefaultRandomizationTypeWithCompletionSelector :: Selector
readAttributeDefaultRandomizationTypeWithCompletionSelector = mkSelector "readAttributeDefaultRandomizationTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeDefaultRandomizationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultRandomizationTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultRandomizationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultRandomizationTypeWithClusterStateCache:endpoint:queue:completion:"

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

