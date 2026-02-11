{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBaseClusterBasic@.
module ObjC.Matter.MTRBaseClusterBasic
  ( MTRBaseClusterBasic
  , IsMTRBaseClusterBasic(..)
  , initWithDevice_endpoint_queue
  , mfgSpecificPingWithParams_completionHandler
  , mfgSpecificPingWithCompletionHandler
  , readAttributeDataModelRevisionWithCompletionHandler
  , subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeVendorNameWithCompletionHandler
  , subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeVendorIDWithCompletionHandler
  , subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeProductNameWithCompletionHandler
  , subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeProductIDWithCompletionHandler
  , subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeNodeLabelWithCompletionHandler
  , writeAttributeNodeLabelWithValue_completionHandler
  , writeAttributeNodeLabelWithValue_params_completionHandler
  , subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLocationWithCompletionHandler
  , writeAttributeLocationWithValue_completionHandler
  , writeAttributeLocationWithValue_params_completionHandler
  , subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLocationWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeHardwareVersionWithCompletionHandler
  , subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeHardwareVersionStringWithCompletionHandler
  , subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSoftwareVersionWithCompletionHandler
  , subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSoftwareVersionStringWithCompletionHandler
  , subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeManufacturingDateWithCompletionHandler
  , subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePartNumberWithCompletionHandler
  , subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeProductURLWithCompletionHandler
  , subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeProductLabelWithCompletionHandler
  , subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSerialNumberWithCompletionHandler
  , subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLocalConfigDisabledWithCompletionHandler
  , writeAttributeLocalConfigDisabledWithValue_completionHandler
  , writeAttributeLocalConfigDisabledWithValue_params_completionHandler
  , subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeReachableWithCompletionHandler
  , subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeReachableWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeUniqueIDWithCompletionHandler
  , subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCapabilityMinimaWithCompletionHandler
  , subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandler
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
  , mfgSpecificPingWithParams_completionHandlerSelector
  , mfgSpecificPingWithCompletionHandlerSelector
  , readAttributeDataModelRevisionWithCompletionHandlerSelector
  , subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeVendorNameWithCompletionHandlerSelector
  , subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeVendorIDWithCompletionHandlerSelector
  , subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeProductNameWithCompletionHandlerSelector
  , subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeProductIDWithCompletionHandlerSelector
  , subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeNodeLabelWithCompletionHandlerSelector
  , writeAttributeNodeLabelWithValue_completionHandlerSelector
  , writeAttributeNodeLabelWithValue_params_completionHandlerSelector
  , subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLocationWithCompletionHandlerSelector
  , writeAttributeLocationWithValue_completionHandlerSelector
  , writeAttributeLocationWithValue_params_completionHandlerSelector
  , subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLocationWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeHardwareVersionWithCompletionHandlerSelector
  , subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeHardwareVersionStringWithCompletionHandlerSelector
  , subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSoftwareVersionWithCompletionHandlerSelector
  , subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSoftwareVersionStringWithCompletionHandlerSelector
  , subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeManufacturingDateWithCompletionHandlerSelector
  , subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePartNumberWithCompletionHandlerSelector
  , subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeProductURLWithCompletionHandlerSelector
  , subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeProductLabelWithCompletionHandlerSelector
  , subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSerialNumberWithCompletionHandlerSelector
  , subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLocalConfigDisabledWithCompletionHandlerSelector
  , writeAttributeLocalConfigDisabledWithValue_completionHandlerSelector
  , writeAttributeLocalConfigDisabledWithValue_params_completionHandlerSelector
  , subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeReachableWithCompletionHandlerSelector
  , subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeReachableWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeUniqueIDWithCompletionHandlerSelector
  , subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCapabilityMinimaWithCompletionHandlerSelector
  , subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandlerSelector
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
initWithDevice_endpoint_queue :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterBasic -> device -> CUShort -> queue -> IO (Id MTRBaseClusterBasic)
initWithDevice_endpoint_queue mtrBaseClusterBasic  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterBasic (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- mfgSpecificPingWithParams:completionHandler:@
mfgSpecificPingWithParams_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsMTRBasicClusterMfgSpecificPingParams params) => mtrBaseClusterBasic -> params -> Ptr () -> IO ()
mfgSpecificPingWithParams_completionHandler mtrBaseClusterBasic  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBasic (mkSelector "mfgSpecificPingWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- mfgSpecificPingWithCompletionHandler:@
mfgSpecificPingWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
mfgSpecificPingWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "mfgSpecificPingWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeDataModelRevisionWithCompletionHandler:@
readAttributeDataModelRevisionWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeDataModelRevisionWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeDataModelRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeDataModelRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeDataModelRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDataModelRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDataModelRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeVendorNameWithCompletionHandler:@
readAttributeVendorNameWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeVendorNameWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeVendorNameWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeVendorIDWithCompletionHandler:@
readAttributeVendorIDWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeVendorIDWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeVendorIDWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeProductNameWithCompletionHandler:@
readAttributeProductNameWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeProductNameWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeProductNameWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeProductNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeProductNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductNameWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeProductIDWithCompletionHandler:@
readAttributeProductIDWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeProductIDWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeProductIDWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeNodeLabelWithCompletionHandler:@
readAttributeNodeLabelWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeNodeLabelWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeNodeLabelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeNodeLabelWithValue:completionHandler:@
writeAttributeNodeLabelWithValue_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSString value) => mtrBaseClusterBasic -> value -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_completionHandler mtrBaseClusterBasic  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBasic (mkSelector "writeAttributeNodeLabelWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeNodeLabelWithValue:params:completionHandler:@
writeAttributeNodeLabelWithValue_params_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBasic -> value -> params -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_params_completionHandler mtrBaseClusterBasic  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBasic (mkSelector "writeAttributeNodeLabelWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeNodeLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeNodeLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNodeLabelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNodeLabelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLocationWithCompletionHandler:@
readAttributeLocationWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeLocationWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeLocationWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLocationWithValue:completionHandler:@
writeAttributeLocationWithValue_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSString value) => mtrBaseClusterBasic -> value -> Ptr () -> IO ()
writeAttributeLocationWithValue_completionHandler mtrBaseClusterBasic  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBasic (mkSelector "writeAttributeLocationWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLocationWithValue:params:completionHandler:@
writeAttributeLocationWithValue_params_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBasic -> value -> params -> Ptr () -> IO ()
writeAttributeLocationWithValue_params_completionHandler mtrBaseClusterBasic  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBasic (mkSelector "writeAttributeLocationWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLocationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeLocationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLocationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLocationWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocationWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLocationWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeHardwareVersionWithCompletionHandler:@
readAttributeHardwareVersionWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeHardwareVersionWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeHardwareVersionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeHardwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeHardwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHardwareVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHardwareVersionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeHardwareVersionStringWithCompletionHandler:@
readAttributeHardwareVersionStringWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeHardwareVersionStringWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeHardwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeHardwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHardwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHardwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSoftwareVersionWithCompletionHandler:@
readAttributeSoftwareVersionWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeSoftwareVersionWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeSoftwareVersionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSoftwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeSoftwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSoftwareVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSoftwareVersionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSoftwareVersionStringWithCompletionHandler:@
readAttributeSoftwareVersionStringWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeSoftwareVersionStringWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSoftwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeSoftwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSoftwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSoftwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeManufacturingDateWithCompletionHandler:@
readAttributeManufacturingDateWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeManufacturingDateWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeManufacturingDateWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeManufacturingDateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeManufacturingDateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeManufacturingDateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeManufacturingDateWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePartNumberWithCompletionHandler:@
readAttributePartNumberWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributePartNumberWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributePartNumberWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePartNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributePartNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePartNumberWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePartNumberWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeProductURLWithCompletionHandler:@
readAttributeProductURLWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeProductURLWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeProductURLWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeProductURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeProductURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductURLWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductURLWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeProductLabelWithCompletionHandler:@
readAttributeProductLabelWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeProductLabelWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeProductLabelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeProductLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeProductLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductLabelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductLabelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSerialNumberWithCompletionHandler:@
readAttributeSerialNumberWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeSerialNumberWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeSerialNumberWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSerialNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeSerialNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSerialNumberWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSerialNumberWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLocalConfigDisabledWithCompletionHandler:@
readAttributeLocalConfigDisabledWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeLocalConfigDisabledWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeLocalConfigDisabledWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLocalConfigDisabledWithValue:completionHandler:@
writeAttributeLocalConfigDisabledWithValue_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber value) => mtrBaseClusterBasic -> value -> Ptr () -> IO ()
writeAttributeLocalConfigDisabledWithValue_completionHandler mtrBaseClusterBasic  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBasic (mkSelector "writeAttributeLocalConfigDisabledWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeLocalConfigDisabledWithValue:params:completionHandler:@
writeAttributeLocalConfigDisabledWithValue_params_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBasic -> value -> params -> Ptr () -> IO ()
writeAttributeLocalConfigDisabledWithValue_params_completionHandler mtrBaseClusterBasic  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBasic (mkSelector "writeAttributeLocalConfigDisabledWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLocalConfigDisabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeLocalConfigDisabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLocalConfigDisabledWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLocalConfigDisabledWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeReachableWithCompletionHandler:@
readAttributeReachableWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeReachableWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeReachableWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeReachableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeReachableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeReachableWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeReachableWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReachableWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeReachableWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeUniqueIDWithCompletionHandler:@
readAttributeUniqueIDWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeUniqueIDWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeUniqueIDWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeUniqueIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeUniqueIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUniqueIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUniqueIDWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCapabilityMinimaWithCompletionHandler:@
readAttributeCapabilityMinimaWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeCapabilityMinimaWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeCapabilityMinimaWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCapabilityMinimaWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeCapabilityMinimaWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCapabilityMinimaWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCapabilityMinimaWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterBasic  completionHandler =
    sendMsg mtrBaseClusterBasic (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBasic (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
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

-- | @Selector@ for @mfgSpecificPingWithParams:completionHandler:@
mfgSpecificPingWithParams_completionHandlerSelector :: Selector
mfgSpecificPingWithParams_completionHandlerSelector = mkSelector "mfgSpecificPingWithParams:completionHandler:"

-- | @Selector@ for @mfgSpecificPingWithCompletionHandler:@
mfgSpecificPingWithCompletionHandlerSelector :: Selector
mfgSpecificPingWithCompletionHandlerSelector = mkSelector "mfgSpecificPingWithCompletionHandler:"

-- | @Selector@ for @readAttributeDataModelRevisionWithCompletionHandler:@
readAttributeDataModelRevisionWithCompletionHandlerSelector :: Selector
readAttributeDataModelRevisionWithCompletionHandlerSelector = mkSelector "readAttributeDataModelRevisionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeDataModelRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDataModelRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDataModelRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeDataModelRevisionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeVendorNameWithCompletionHandler:@
readAttributeVendorNameWithCompletionHandlerSelector :: Selector
readAttributeVendorNameWithCompletionHandlerSelector = mkSelector "readAttributeVendorNameWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeVendorIDWithCompletionHandler:@
readAttributeVendorIDWithCompletionHandlerSelector :: Selector
readAttributeVendorIDWithCompletionHandlerSelector = mkSelector "readAttributeVendorIDWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeProductNameWithCompletionHandler:@
readAttributeProductNameWithCompletionHandlerSelector :: Selector
readAttributeProductNameWithCompletionHandlerSelector = mkSelector "readAttributeProductNameWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeProductNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeProductNameWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeProductIDWithCompletionHandler:@
readAttributeProductIDWithCompletionHandlerSelector :: Selector
readAttributeProductIDWithCompletionHandlerSelector = mkSelector "readAttributeProductIDWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeNodeLabelWithCompletionHandler:@
readAttributeNodeLabelWithCompletionHandlerSelector :: Selector
readAttributeNodeLabelWithCompletionHandlerSelector = mkSelector "readAttributeNodeLabelWithCompletionHandler:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:completionHandler:@
writeAttributeNodeLabelWithValue_completionHandlerSelector :: Selector
writeAttributeNodeLabelWithValue_completionHandlerSelector = mkSelector "writeAttributeNodeLabelWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:params:completionHandler:@
writeAttributeNodeLabelWithValue_params_completionHandlerSelector :: Selector
writeAttributeNodeLabelWithValue_params_completionHandlerSelector = mkSelector "writeAttributeNodeLabelWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeNodeLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeLabelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeNodeLabelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLocationWithCompletionHandler:@
readAttributeLocationWithCompletionHandlerSelector :: Selector
readAttributeLocationWithCompletionHandlerSelector = mkSelector "readAttributeLocationWithCompletionHandler:"

-- | @Selector@ for @writeAttributeLocationWithValue:completionHandler:@
writeAttributeLocationWithValue_completionHandlerSelector :: Selector
writeAttributeLocationWithValue_completionHandlerSelector = mkSelector "writeAttributeLocationWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeLocationWithValue:params:completionHandler:@
writeAttributeLocationWithValue_params_completionHandlerSelector :: Selector
writeAttributeLocationWithValue_params_completionHandlerSelector = mkSelector "writeAttributeLocationWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeLocationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLocationWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLocationWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLocationWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeHardwareVersionWithCompletionHandler:@
readAttributeHardwareVersionWithCompletionHandlerSelector :: Selector
readAttributeHardwareVersionWithCompletionHandlerSelector = mkSelector "readAttributeHardwareVersionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeHardwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHardwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHardwareVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeHardwareVersionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeHardwareVersionStringWithCompletionHandler:@
readAttributeHardwareVersionStringWithCompletionHandlerSelector :: Selector
readAttributeHardwareVersionStringWithCompletionHandlerSelector = mkSelector "readAttributeHardwareVersionStringWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeHardwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHardwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHardwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeHardwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionWithCompletionHandler:@
readAttributeSoftwareVersionWithCompletionHandlerSelector :: Selector
readAttributeSoftwareVersionWithCompletionHandlerSelector = mkSelector "readAttributeSoftwareVersionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSoftwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSoftwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSoftwareVersionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionStringWithCompletionHandler:@
readAttributeSoftwareVersionStringWithCompletionHandlerSelector :: Selector
readAttributeSoftwareVersionStringWithCompletionHandlerSelector = mkSelector "readAttributeSoftwareVersionStringWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSoftwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSoftwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSoftwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeManufacturingDateWithCompletionHandler:@
readAttributeManufacturingDateWithCompletionHandlerSelector :: Selector
readAttributeManufacturingDateWithCompletionHandlerSelector = mkSelector "readAttributeManufacturingDateWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeManufacturingDateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeManufacturingDateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeManufacturingDateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeManufacturingDateWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePartNumberWithCompletionHandler:@
readAttributePartNumberWithCompletionHandlerSelector :: Selector
readAttributePartNumberWithCompletionHandlerSelector = mkSelector "readAttributePartNumberWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePartNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePartNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePartNumberWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePartNumberWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeProductURLWithCompletionHandler:@
readAttributeProductURLWithCompletionHandlerSelector :: Selector
readAttributeProductURLWithCompletionHandlerSelector = mkSelector "readAttributeProductURLWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeProductURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductURLWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeProductURLWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeProductLabelWithCompletionHandler:@
readAttributeProductLabelWithCompletionHandlerSelector :: Selector
readAttributeProductLabelWithCompletionHandlerSelector = mkSelector "readAttributeProductLabelWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeProductLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductLabelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeProductLabelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSerialNumberWithCompletionHandler:@
readAttributeSerialNumberWithCompletionHandlerSelector :: Selector
readAttributeSerialNumberWithCompletionHandlerSelector = mkSelector "readAttributeSerialNumberWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSerialNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSerialNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSerialNumberWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSerialNumberWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLocalConfigDisabledWithCompletionHandler:@
readAttributeLocalConfigDisabledWithCompletionHandlerSelector :: Selector
readAttributeLocalConfigDisabledWithCompletionHandlerSelector = mkSelector "readAttributeLocalConfigDisabledWithCompletionHandler:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:completionHandler:@
writeAttributeLocalConfigDisabledWithValue_completionHandlerSelector :: Selector
writeAttributeLocalConfigDisabledWithValue_completionHandlerSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:params:completionHandler:@
writeAttributeLocalConfigDisabledWithValue_params_completionHandlerSelector :: Selector
writeAttributeLocalConfigDisabledWithValue_params_completionHandlerSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeLocalConfigDisabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocalConfigDisabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocalConfigDisabledWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLocalConfigDisabledWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeReachableWithCompletionHandler:@
readAttributeReachableWithCompletionHandlerSelector :: Selector
readAttributeReachableWithCompletionHandlerSelector = mkSelector "readAttributeReachableWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeReachableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReachableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReachableWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeReachableWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeReachableWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeReachableWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeUniqueIDWithCompletionHandler:@
readAttributeUniqueIDWithCompletionHandlerSelector :: Selector
readAttributeUniqueIDWithCompletionHandlerSelector = mkSelector "readAttributeUniqueIDWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeUniqueIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUniqueIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUniqueIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeUniqueIDWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCapabilityMinimaWithCompletionHandler:@
readAttributeCapabilityMinimaWithCompletionHandlerSelector :: Selector
readAttributeCapabilityMinimaWithCompletionHandlerSelector = mkSelector "readAttributeCapabilityMinimaWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCapabilityMinimaWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCapabilityMinimaWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCapabilityMinimaWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCapabilityMinimaWithAttributeCache:endpoint:queue:completionHandler:"

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

