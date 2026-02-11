{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBaseClusterOtaSoftwareUpdateRequestor@.
module ObjC.Matter.MTRBaseClusterOtaSoftwareUpdateRequestor
  ( MTRBaseClusterOtaSoftwareUpdateRequestor
  , IsMTRBaseClusterOtaSoftwareUpdateRequestor(..)
  , initWithDevice_endpoint_queue
  , announceOtaProviderWithParams_completionHandler
  , readAttributeDefaultOtaProvidersWithParams_completionHandler
  , writeAttributeDefaultOtaProvidersWithValue_completionHandler
  , writeAttributeDefaultOtaProvidersWithValue_params_completionHandler
  , subscribeAttributeDefaultOtaProvidersWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeDefaultOtaProvidersWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeUpdatePossibleWithCompletionHandler
  , subscribeAttributeUpdatePossibleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeUpdatePossibleWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeUpdateStateWithCompletionHandler
  , subscribeAttributeUpdateStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeUpdateStateWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeUpdateStateProgressWithCompletionHandler
  , subscribeAttributeUpdateStateProgressWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeUpdateStateProgressWithAttributeCache_endpoint_queue_completionHandler
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
  , initWithDevice_endpoint_queueSelector
  , announceOtaProviderWithParams_completionHandlerSelector
  , readAttributeDefaultOtaProvidersWithParams_completionHandlerSelector
  , writeAttributeDefaultOtaProvidersWithValue_completionHandlerSelector
  , writeAttributeDefaultOtaProvidersWithValue_params_completionHandlerSelector
  , subscribeAttributeDefaultOtaProvidersWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeDefaultOtaProvidersWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeUpdatePossibleWithCompletionHandlerSelector
  , subscribeAttributeUpdatePossibleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeUpdatePossibleWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeUpdateStateWithCompletionHandlerSelector
  , subscribeAttributeUpdateStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeUpdateStateWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeUpdateStateProgressWithCompletionHandlerSelector
  , subscribeAttributeUpdateStateProgressWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeUpdateStateProgressWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterOtaSoftwareUpdateRequestor -> device -> CUShort -> queue -> IO (Id MTRBaseClusterOtaSoftwareUpdateRequestor)
initWithDevice_endpoint_queue mtrBaseClusterOtaSoftwareUpdateRequestor  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- announceOtaProviderWithParams:completionHandler:@
announceOtaProviderWithParams_completionHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> params -> Ptr () -> IO ()
announceOtaProviderWithParams_completionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "announceOtaProviderWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeDefaultOtaProvidersWithParams:completionHandler:@
readAttributeDefaultOtaProvidersWithParams_completionHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsMTRReadParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> params -> Ptr () -> IO ()
readAttributeDefaultOtaProvidersWithParams_completionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "readAttributeDefaultOtaProvidersWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeDefaultOtaProvidersWithValue:completionHandler:@
writeAttributeDefaultOtaProvidersWithValue_completionHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsNSArray value) => mtrBaseClusterOtaSoftwareUpdateRequestor -> value -> Ptr () -> IO ()
writeAttributeDefaultOtaProvidersWithValue_completionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "writeAttributeDefaultOtaProvidersWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeDefaultOtaProvidersWithValue:params:completionHandler:@
writeAttributeDefaultOtaProvidersWithValue_params_completionHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> value -> params -> Ptr () -> IO ()
writeAttributeDefaultOtaProvidersWithValue_params_completionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "writeAttributeDefaultOtaProvidersWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeDefaultOtaProvidersWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOtaProvidersWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultOtaProvidersWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOtaSoftwareUpdateRequestor  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "subscribeAttributeDefaultOtaProvidersWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDefaultOtaProvidersWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDefaultOtaProvidersWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultOtaProvidersWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOtaSoftwareUpdateRequestor"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDefaultOtaProvidersWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeUpdatePossibleWithCompletionHandler:@
readAttributeUpdatePossibleWithCompletionHandler :: IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor => mtrBaseClusterOtaSoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeUpdatePossibleWithCompletionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  completionHandler =
    sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "readAttributeUpdatePossibleWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeUpdatePossibleWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdatePossibleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUpdatePossibleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOtaSoftwareUpdateRequestor  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "subscribeAttributeUpdatePossibleWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUpdatePossibleWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUpdatePossibleWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUpdatePossibleWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOtaSoftwareUpdateRequestor"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUpdatePossibleWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeUpdateStateWithCompletionHandler:@
readAttributeUpdateStateWithCompletionHandler :: IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor => mtrBaseClusterOtaSoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeUpdateStateWithCompletionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  completionHandler =
    sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "readAttributeUpdateStateWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeUpdateStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdateStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUpdateStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOtaSoftwareUpdateRequestor  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "subscribeAttributeUpdateStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUpdateStateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUpdateStateWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUpdateStateWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOtaSoftwareUpdateRequestor"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUpdateStateWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeUpdateStateProgressWithCompletionHandler:@
readAttributeUpdateStateProgressWithCompletionHandler :: IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor => mtrBaseClusterOtaSoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeUpdateStateProgressWithCompletionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  completionHandler =
    sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "readAttributeUpdateStateProgressWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeUpdateStateProgressWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdateStateProgressWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUpdateStateProgressWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOtaSoftwareUpdateRequestor  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "subscribeAttributeUpdateStateProgressWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUpdateStateProgressWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUpdateStateProgressWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUpdateStateProgressWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOtaSoftwareUpdateRequestor"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUpdateStateProgressWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor => mtrBaseClusterOtaSoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  completionHandler =
    sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOtaSoftwareUpdateRequestor  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOtaSoftwareUpdateRequestor"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor => mtrBaseClusterOtaSoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  completionHandler =
    sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOtaSoftwareUpdateRequestor  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOtaSoftwareUpdateRequestor"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor => mtrBaseClusterOtaSoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  completionHandler =
    sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOtaSoftwareUpdateRequestor  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOtaSoftwareUpdateRequestor"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor => mtrBaseClusterOtaSoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  completionHandler =
    sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOtaSoftwareUpdateRequestor  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOtaSoftwareUpdateRequestor"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor => mtrBaseClusterOtaSoftwareUpdateRequestor -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterOtaSoftwareUpdateRequestor  completionHandler =
    sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOtaSoftwareUpdateRequestor mtrBaseClusterOtaSoftwareUpdateRequestor, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOtaSoftwareUpdateRequestor -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOtaSoftwareUpdateRequestor  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOtaSoftwareUpdateRequestor (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOtaSoftwareUpdateRequestor"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @announceOtaProviderWithParams:completionHandler:@
announceOtaProviderWithParams_completionHandlerSelector :: Selector
announceOtaProviderWithParams_completionHandlerSelector = mkSelector "announceOtaProviderWithParams:completionHandler:"

-- | @Selector@ for @readAttributeDefaultOtaProvidersWithParams:completionHandler:@
readAttributeDefaultOtaProvidersWithParams_completionHandlerSelector :: Selector
readAttributeDefaultOtaProvidersWithParams_completionHandlerSelector = mkSelector "readAttributeDefaultOtaProvidersWithParams:completionHandler:"

-- | @Selector@ for @writeAttributeDefaultOtaProvidersWithValue:completionHandler:@
writeAttributeDefaultOtaProvidersWithValue_completionHandlerSelector :: Selector
writeAttributeDefaultOtaProvidersWithValue_completionHandlerSelector = mkSelector "writeAttributeDefaultOtaProvidersWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeDefaultOtaProvidersWithValue:params:completionHandler:@
writeAttributeDefaultOtaProvidersWithValue_params_completionHandlerSelector :: Selector
writeAttributeDefaultOtaProvidersWithValue_params_completionHandlerSelector = mkSelector "writeAttributeDefaultOtaProvidersWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeDefaultOtaProvidersWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOtaProvidersWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDefaultOtaProvidersWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultOtaProvidersWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultOtaProvidersWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDefaultOtaProvidersWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeDefaultOtaProvidersWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeDefaultOtaProvidersWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeUpdatePossibleWithCompletionHandler:@
readAttributeUpdatePossibleWithCompletionHandlerSelector :: Selector
readAttributeUpdatePossibleWithCompletionHandlerSelector = mkSelector "readAttributeUpdatePossibleWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeUpdatePossibleWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdatePossibleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUpdatePossibleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUpdatePossibleWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUpdatePossibleWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUpdatePossibleWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeUpdatePossibleWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeUpdatePossibleWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeUpdateStateWithCompletionHandler:@
readAttributeUpdateStateWithCompletionHandlerSelector :: Selector
readAttributeUpdateStateWithCompletionHandlerSelector = mkSelector "readAttributeUpdateStateWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeUpdateStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdateStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUpdateStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUpdateStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUpdateStateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUpdateStateWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeUpdateStateWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeUpdateStateWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeUpdateStateProgressWithCompletionHandler:@
readAttributeUpdateStateProgressWithCompletionHandlerSelector :: Selector
readAttributeUpdateStateProgressWithCompletionHandlerSelector = mkSelector "readAttributeUpdateStateProgressWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeUpdateStateProgressWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUpdateStateProgressWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUpdateStateProgressWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUpdateStateProgressWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUpdateStateProgressWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUpdateStateProgressWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeUpdateStateProgressWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeUpdateStateProgressWithAttributeCache:endpoint:queue:completionHandler:"

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

