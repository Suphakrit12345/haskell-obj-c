{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Price
--
-- The Commodity Price Cluster provides the mechanism for communicating Gas, Energy, or Water pricing information within the premises.
--
-- Generated bindings for @MTRBaseClusterCommodityPrice@.
module ObjC.Matter.MTRBaseClusterCommodityPrice
  ( MTRBaseClusterCommodityPrice
  , IsMTRBaseClusterCommodityPrice(..)
  , getDetailedPriceRequestWithParams_completion
  , getDetailedForecastRequestWithParams_completion
  , readAttributeTariffUnitWithCompletion
  , subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrencyWithCompletion
  , subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrencyWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentPriceWithCompletion
  , subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completion
  , readAttributePriceForecastWithCompletion
  , subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandler
  , readAttributePriceForecastWithClusterStateCache_endpoint_queue_completion
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
  , getDetailedPriceRequestWithParams_completionSelector
  , getDetailedForecastRequestWithParams_completionSelector
  , readAttributeTariffUnitWithCompletionSelector
  , subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrencyWithCompletionSelector
  , subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrencyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentPriceWithCompletionSelector
  , subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePriceForecastWithCompletionSelector
  , subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePriceForecastWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command GetDetailedPriceRequest
--
-- Upon receipt, this SHALL generate a GetDetailedPrice Response command.
--
-- ObjC selector: @- getDetailedPriceRequestWithParams:completion:@
getDetailedPriceRequestWithParams_completion :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRCommodityPriceClusterGetDetailedPriceRequestParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> IO ()
getDetailedPriceRequestWithParams_completion mtrBaseClusterCommodityPrice  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityPrice (mkSelector "getDetailedPriceRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command GetDetailedForecastRequest
--
-- Upon receipt, this SHALL generate a GetDetailedForecast Response command.
--
-- ObjC selector: @- getDetailedForecastRequestWithParams:completion:@
getDetailedForecastRequestWithParams_completion :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRCommodityPriceClusterGetDetailedForecastRequestParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> IO ()
getDetailedForecastRequestWithParams_completion mtrBaseClusterCommodityPrice  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityPrice (mkSelector "getDetailedForecastRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeTariffUnitWithCompletion mtrBaseClusterCommodityPrice  completion =
    sendMsg mtrBaseClusterCommodityPrice (mkSelector "readAttributeTariffUnitWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityPrice (mkSelector "subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrencyWithCompletion:@
readAttributeCurrencyWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeCurrencyWithCompletion mtrBaseClusterCommodityPrice  completion =
    sendMsg mtrBaseClusterCommodityPrice (mkSelector "readAttributeCurrencyWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityPrice (mkSelector "subscribeAttributeCurrencyWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrencyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrencyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrencyWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentPriceWithCompletion:@
readAttributeCurrentPriceWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeCurrentPriceWithCompletion mtrBaseClusterCommodityPrice  completion =
    sendMsg mtrBaseClusterCommodityPrice (mkSelector "readAttributeCurrentPriceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentPriceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityPrice (mkSelector "subscribeAttributeCurrentPriceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentPriceWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentPriceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePriceForecastWithCompletion:@
readAttributePriceForecastWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributePriceForecastWithCompletion mtrBaseClusterCommodityPrice  completion =
    sendMsg mtrBaseClusterCommodityPrice (mkSelector "readAttributePriceForecastWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePriceForecastWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityPrice (mkSelector "subscribeAttributePriceForecastWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePriceForecastWithClusterStateCache:endpoint:queue:completion:@
readAttributePriceForecastWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePriceForecastWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePriceForecastWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterCommodityPrice  completion =
    sendMsg mtrBaseClusterCommodityPrice (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityPrice (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterCommodityPrice  completion =
    sendMsg mtrBaseClusterCommodityPrice (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityPrice (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterCommodityPrice  completion =
    sendMsg mtrBaseClusterCommodityPrice (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityPrice (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterCommodityPrice  completion =
    sendMsg mtrBaseClusterCommodityPrice (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityPrice (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterCommodityPrice  completion =
    sendMsg mtrBaseClusterCommodityPrice (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityPrice (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> IO (Id MTRBaseClusterCommodityPrice)
init_ mtrBaseClusterCommodityPrice  =
    sendMsg mtrBaseClusterCommodityPrice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterCommodityPrice)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterCommodityPrice -> device -> endpointID -> queue -> IO (Id MTRBaseClusterCommodityPrice)
initWithDevice_endpointID_queue mtrBaseClusterCommodityPrice  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterCommodityPrice (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getDetailedPriceRequestWithParams:completion:@
getDetailedPriceRequestWithParams_completionSelector :: Selector
getDetailedPriceRequestWithParams_completionSelector = mkSelector "getDetailedPriceRequestWithParams:completion:"

-- | @Selector@ for @getDetailedForecastRequestWithParams:completion:@
getDetailedForecastRequestWithParams_completionSelector :: Selector
getDetailedForecastRequestWithParams_completionSelector = mkSelector "getDetailedForecastRequestWithParams:completion:"

-- | @Selector@ for @readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletionSelector :: Selector
readAttributeTariffUnitWithCompletionSelector = mkSelector "readAttributeTariffUnitWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrencyWithCompletion:@
readAttributeCurrencyWithCompletionSelector :: Selector
readAttributeCurrencyWithCompletionSelector = mkSelector "readAttributeCurrencyWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrencyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrencyWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrencyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrencyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentPriceWithCompletion:@
readAttributeCurrentPriceWithCompletionSelector :: Selector
readAttributeCurrentPriceWithCompletionSelector = mkSelector "readAttributeCurrentPriceWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentPriceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentPriceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentPriceWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentPriceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePriceForecastWithCompletion:@
readAttributePriceForecastWithCompletionSelector :: Selector
readAttributePriceForecastWithCompletionSelector = mkSelector "readAttributePriceForecastWithCompletion:"

-- | @Selector@ for @subscribeAttributePriceForecastWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePriceForecastWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePriceForecastWithClusterStateCache:endpoint:queue:completion:@
readAttributePriceForecastWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePriceForecastWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePriceForecastWithClusterStateCache:endpoint:queue:completion:"

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

