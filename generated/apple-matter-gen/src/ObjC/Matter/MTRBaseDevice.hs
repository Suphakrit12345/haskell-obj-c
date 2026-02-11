{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBaseDevice@.
module ObjC.Matter.MTRBaseDevice
  ( MTRBaseDevice
  , IsMTRBaseDevice(..)
  , init_
  , new
  , deviceWithNodeID_controller
  , subscribeWithQueue_params_clusterStateCacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduled
  , readAttributesWithEndpointID_clusterID_attributeID_params_queue_completion
  , readAttributePaths_eventPaths_params_queue_completion
  , writeAttributeWithEndpointID_clusterID_attributeID_value_timedWriteTimeout_queue_completion
  , invokeCommandWithEndpointID_clusterID_commandID_commandFields_timedInvokeTimeout_queue_completion
  , subscribeToAttributesWithEndpointID_clusterID_attributeID_params_queue_reportHandler_subscriptionEstablished
  , subscribeToAttributePaths_eventPaths_params_queue_reportHandler_subscriptionEstablished_resubscriptionScheduled
  , deregisterReportHandlersWithQueue_completion
  , openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completion
  , openCommissioningWindowWithDiscriminator_duration_queue_completion
  , readEventsWithEndpointID_clusterID_eventID_params_queue_completion
  , subscribeToEventsWithEndpointID_clusterID_eventID_params_queue_reportHandler_subscriptionEstablished
  , downloadLogOfType_timeout_queue_completion
  , subscribeWithQueue_minInterval_maxInterval_params_cacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduled
  , readAttributeWithEndpointId_clusterId_attributeId_params_clientQueue_completion
  , writeAttributeWithEndpointId_clusterId_attributeId_value_timedWriteTimeout_clientQueue_completion
  , invokeCommandWithEndpointId_clusterId_commandId_commandFields_timedInvokeTimeout_clientQueue_completion
  , subscribeAttributeWithEndpointId_clusterId_attributeId_minInterval_maxInterval_params_clientQueue_reportHandler_subscriptionEstablished
  , deregisterReportHandlersWithClientQueue_completion
  , sessionTransportType
  , initSelector
  , newSelector
  , deviceWithNodeID_controllerSelector
  , subscribeWithQueue_params_clusterStateCacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduledSelector
  , readAttributesWithEndpointID_clusterID_attributeID_params_queue_completionSelector
  , readAttributePaths_eventPaths_params_queue_completionSelector
  , writeAttributeWithEndpointID_clusterID_attributeID_value_timedWriteTimeout_queue_completionSelector
  , invokeCommandWithEndpointID_clusterID_commandID_commandFields_timedInvokeTimeout_queue_completionSelector
  , subscribeToAttributesWithEndpointID_clusterID_attributeID_params_queue_reportHandler_subscriptionEstablishedSelector
  , subscribeToAttributePaths_eventPaths_params_queue_reportHandler_subscriptionEstablished_resubscriptionScheduledSelector
  , deregisterReportHandlersWithQueue_completionSelector
  , openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completionSelector
  , openCommissioningWindowWithDiscriminator_duration_queue_completionSelector
  , readEventsWithEndpointID_clusterID_eventID_params_queue_completionSelector
  , subscribeToEventsWithEndpointID_clusterID_eventID_params_queue_reportHandler_subscriptionEstablishedSelector
  , downloadLogOfType_timeout_queue_completionSelector
  , subscribeWithQueue_minInterval_maxInterval_params_cacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduledSelector
  , readAttributeWithEndpointId_clusterId_attributeId_params_clientQueue_completionSelector
  , writeAttributeWithEndpointId_clusterId_attributeId_value_timedWriteTimeout_clientQueue_completionSelector
  , invokeCommandWithEndpointId_clusterId_commandId_commandFields_timedInvokeTimeout_clientQueue_completionSelector
  , subscribeAttributeWithEndpointId_clusterId_attributeId_minInterval_maxInterval_params_clientQueue_reportHandler_subscriptionEstablishedSelector
  , deregisterReportHandlersWithClientQueue_completionSelector
  , sessionTransportTypeSelector

  -- * Enum types
  , MTRDiagnosticLogType(MTRDiagnosticLogType)
  , pattern MTRDiagnosticLogTypeEndUserSupport
  , pattern MTRDiagnosticLogTypeNetworkDiagnostics
  , pattern MTRDiagnosticLogTypeCrash
  , MTRTransportType(MTRTransportType)
  , pattern MTRTransportTypeUndefined
  , pattern MTRTransportTypeUDP
  , pattern MTRTransportTypeBLE
  , pattern MTRTransportTypeTCP

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
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRBaseDevice mtrBaseDevice => mtrBaseDevice -> IO (Id MTRBaseDevice)
init_ mtrBaseDevice  =
    sendMsg mtrBaseDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseDevice)
new  =
  do
    cls' <- getRequiredClass "MTRBaseDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Create a device object with the given node id and controller.  This will always succeed, even if there is no such node id on the controller's fabric, but attempts to actually use the MTRBaseDevice will fail (asynchronously) in that case.
--
-- ObjC selector: @+ deviceWithNodeID:controller:@
deviceWithNodeID_controller :: (IsNSNumber nodeID, IsMTRDeviceController controller) => nodeID -> controller -> IO (Id MTRBaseDevice)
deviceWithNodeID_controller nodeID controller =
  do
    cls' <- getRequiredClass "MTRBaseDevice"
    withObjCPtr nodeID $ \raw_nodeID ->
      withObjCPtr controller $ \raw_controller ->
        sendClassMsg cls' (mkSelector "deviceWithNodeID:controller:") (retPtr retVoid) [argPtr (castPtr raw_nodeID :: Ptr ()), argPtr (castPtr raw_controller :: Ptr ())] >>= retainedObject . castPtr

-- | Subscribe to receive attribute reports for everything (all endpoints, all clusters, all attributes, all events) on the device.
--
-- A non-nil attribute cache container will cache attribute values, retrievable through the designated attribute cache container.
--
-- attributeReportHandler will be called any time a data update is available (with a non-nil "value")
--
-- The array passed to attributeReportHandler will contain MTRAttributeReport instances.  Errors for specific paths, not the whole subscription, will be reported via those objects.
--
-- eventReportHandler will be called any time an event is reported (with a non-nil "value")
--
-- The array passed to eventReportHandler will contain MTREventReport instances.  Errors for specific paths, not the whole subscription, will be reported via those objects.
--
-- errorHandler will be called any time there is an error for the entire subscription (with a non-nil "error"), and terminate the subscription.  This will generally not be invoked if auto-resubscription is enabled, unless there is a fatal error during a resubscription attempt.
--
-- Both report handlers are not supported over XPC at the moment.
--
-- The subscriptionEstablished block, if not nil, will be called once the subscription is established.  This will be _after_ the first (priming) call to both report handlers.  Note that if the MTRSubscribeParams are set to automatically resubscribe this can end up being called more than once.
--
-- The resubscriptionScheduled block, if not nil, will be called if auto-resubscription is enabled, subscription loss is detected, and a resubscription is scheduled.  This can be called multiple times in a row without an intervening subscriptionEstablished call if the resubscription attempts fail.
--
-- ObjC selector: @- subscribeWithQueue:params:clusterStateCacheContainer:attributeReportHandler:eventReportHandler:errorHandler:subscriptionEstablished:resubscriptionScheduled:@
subscribeWithQueue_params_clusterStateCacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduled :: (IsMTRBaseDevice mtrBaseDevice, IsNSObject queue, IsMTRSubscribeParams params, IsMTRClusterStateCacheContainer clusterStateCacheContainer) => mtrBaseDevice -> queue -> params -> clusterStateCacheContainer -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO ()
subscribeWithQueue_params_clusterStateCacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduled mtrBaseDevice  queue params clusterStateCacheContainer attributeReportHandler eventReportHandler errorHandler subscriptionEstablished resubscriptionScheduled =
  withObjCPtr queue $ \raw_queue ->
    withObjCPtr params $ \raw_params ->
      withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
          sendMsg mtrBaseDevice (mkSelector "subscribeWithQueue:params:clusterStateCacheContainer:attributeReportHandler:eventReportHandler:errorHandler:subscriptionEstablished:resubscriptionScheduled:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr attributeReportHandler :: Ptr ()), argPtr (castPtr eventReportHandler :: Ptr ()), argPtr (castPtr errorHandler :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr resubscriptionScheduled :: Ptr ())]

-- | Reads attributes from the device.
--
-- Nil values for endpointID, clusterID, attributeID indicate wildcards (e.g. nil attributeID means "read all the attributes from the endpoint(s) and cluster(s) that match endpointID/clusterID").
--
-- If all of endpointID, clusterID, attributeID are non-nil, a single attribute will be read.
--
-- If all of endpointID, clusterID, attributeID are nil, all attributes on the device will be read.
--
-- A non-nil attributeID along with a nil clusterID will only succeed if the attribute ID is for a global attribute that applies to all clusters.
--
-- The completion will be called with an error if the entire read interaction fails. Otherwise it will be called with values, which may be empty (e.g. if no paths matched the wildcard) or may include per-path errors if particular paths failed.
--
-- ObjC selector: @- readAttributesWithEndpointID:clusterID:attributeID:params:queue:completion:@
readAttributesWithEndpointID_clusterID_attributeID_params_queue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber attributeID, IsMTRReadParams params, IsNSObject queue) => mtrBaseDevice -> endpointID -> clusterID -> attributeID -> params -> queue -> Ptr () -> IO ()
readAttributesWithEndpointID_clusterID_attributeID_params_queue_completion mtrBaseDevice  endpointID clusterID attributeID params queue completion =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr clusterID $ \raw_clusterID ->
      withObjCPtr attributeID $ \raw_attributeID ->
        withObjCPtr params $ \raw_params ->
          withObjCPtr queue $ \raw_queue ->
              sendMsg mtrBaseDevice (mkSelector "readAttributesWithEndpointID:clusterID:attributeID:params:queue:completion:") retVoid [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_attributeID :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Reads multiple attribute or event paths from the device.
--
-- Nil is treated as an empty array for attributePaths and eventPaths.
--
-- Lists of attribute and event paths to read can be provided via attributePaths and eventPaths.
--
-- The completion will be called with an error if the entire read interaction fails. Otherwise it will be called with an array of values. This array may be empty (e.g. if no paths matched the wildcard paths passed in, or if empty lists of paths were passed in) or may include per-path errors if particular paths failed.
--
-- If the sum of the lengths of attributePaths and eventPaths exceeds 9, the read may fail due to the device not supporting that many read paths.
--
-- ObjC selector: @- readAttributePaths:eventPaths:params:queue:completion:@
readAttributePaths_eventPaths_params_queue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSArray attributePaths, IsNSArray eventPaths, IsMTRReadParams params, IsNSObject queue) => mtrBaseDevice -> attributePaths -> eventPaths -> params -> queue -> Ptr () -> IO ()
readAttributePaths_eventPaths_params_queue_completion mtrBaseDevice  attributePaths eventPaths params queue completion =
  withObjCPtr attributePaths $ \raw_attributePaths ->
    withObjCPtr eventPaths $ \raw_eventPaths ->
      withObjCPtr params $ \raw_params ->
        withObjCPtr queue $ \raw_queue ->
            sendMsg mtrBaseDevice (mkSelector "readAttributePaths:eventPaths:params:queue:completion:") retVoid [argPtr (castPtr raw_attributePaths :: Ptr ()), argPtr (castPtr raw_eventPaths :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Write to attribute in a designated attribute path
--
-- @value@ — A data-value NSDictionary object as described in                    MTRDeviceResponseHandler.
--
-- @timeoutMs@ — timeout in milliseconds for timed write, or nil.
--
-- @completion@ — response handler will receive either values or error.
--
-- A path-specific error status will get turned into an error                    passed to the completion, so values will only be passed in                    when the write succeeds.  In that case, values will have                    the format documented in the definition of                    MTRDeviceResponseHandler and will be an array with a single element                    which is a dictionary that has a MTRAttributePathKey entry in it, whose value                    is the attribute path that was successfully written to.
--
-- ObjC selector: @- writeAttributeWithEndpointID:clusterID:attributeID:value:timedWriteTimeout:queue:completion:@
writeAttributeWithEndpointID_clusterID_attributeID_value_timedWriteTimeout_queue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber attributeID, IsNSNumber timeoutMs, IsNSObject queue) => mtrBaseDevice -> endpointID -> clusterID -> attributeID -> RawId -> timeoutMs -> queue -> Ptr () -> IO ()
writeAttributeWithEndpointID_clusterID_attributeID_value_timedWriteTimeout_queue_completion mtrBaseDevice  endpointID clusterID attributeID value timeoutMs queue completion =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr clusterID $ \raw_clusterID ->
      withObjCPtr attributeID $ \raw_attributeID ->
        withObjCPtr timeoutMs $ \raw_timeoutMs ->
          withObjCPtr queue $ \raw_queue ->
              sendMsg mtrBaseDevice (mkSelector "writeAttributeWithEndpointID:clusterID:attributeID:value:timedWriteTimeout:queue:completion:") retVoid [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_attributeID :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_timeoutMs :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Invoke a command with a designated command path
--
-- @commandFields@ — command fields object. The object must be a data-value NSDictionary object                      as described in the MTRDeviceResponseHandler.                      The attribute must be a Structure, i.e.,                      the NSDictionary MTRTypeKey key must have the value MTRStructureValueType.
--
-- @timeoutMs@ — timeout in milliseconds for timed invoke, or nil.
--
-- @completion@ — response handler will receive either values or error.  A                    path-specific error status from the command invocation                    will result in an error being passed to the completion, so                    values will only be passed in when the command succeeds.
--
-- ObjC selector: @- invokeCommandWithEndpointID:clusterID:commandID:commandFields:timedInvokeTimeout:queue:completion:@
invokeCommandWithEndpointID_clusterID_commandID_commandFields_timedInvokeTimeout_queue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber commandID, IsNSNumber timeoutMs, IsNSObject queue) => mtrBaseDevice -> endpointID -> clusterID -> commandID -> RawId -> timeoutMs -> queue -> Ptr () -> IO ()
invokeCommandWithEndpointID_clusterID_commandID_commandFields_timedInvokeTimeout_queue_completion mtrBaseDevice  endpointID clusterID commandID commandFields timeoutMs queue completion =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr clusterID $ \raw_clusterID ->
      withObjCPtr commandID $ \raw_commandID ->
        withObjCPtr timeoutMs $ \raw_timeoutMs ->
          withObjCPtr queue $ \raw_queue ->
              sendMsg mtrBaseDevice (mkSelector "invokeCommandWithEndpointID:clusterID:commandID:commandFields:timedInvokeTimeout:queue:completion:") retVoid [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_commandID :: Ptr ()), argPtr (castPtr (unRawId commandFields) :: Ptr ()), argPtr (castPtr raw_timeoutMs :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Subscribes to the specified attributes on the device.
--
-- Nil values for endpointID, clusterID, attributeID indicate wildcards (e.g. nil attributeID means "subscribe to all the attributes from the endpoint(s) and cluster(s) that match endpointID/clusterID").
--
-- If all of endpointID, clusterID, attributeID are non-nil, a single attribute will be subscribed to.
--
-- If all of endpointID, clusterID, attributeID are nil, all attributes on the device will be subscribed to.
--
-- A non-nil attributeID along with a nil clusterID will only succeed if the attribute ID is for a global attribute that applies to all clusters.
--
-- The reportHandler will be called with an error if the subscription fails entirely.
--
-- The reportHandler will be called with arrays of response-value dictionaries (which may be data or errors) as path-specific data is received.
--
-- subscriptionEstablished will be called when the subscription is first successfully established (after the initial set of data reports has been delivered to reportHandler).  If params allow automatic resubscription, it will be called any time resubscription succeeds.
--
-- ObjC selector: @- subscribeToAttributesWithEndpointID:clusterID:attributeID:params:queue:reportHandler:subscriptionEstablished:@
subscribeToAttributesWithEndpointID_clusterID_attributeID_params_queue_reportHandler_subscriptionEstablished :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber attributeID, IsMTRSubscribeParams params, IsNSObject queue) => mtrBaseDevice -> endpointID -> clusterID -> attributeID -> params -> queue -> Ptr () -> Ptr () -> IO ()
subscribeToAttributesWithEndpointID_clusterID_attributeID_params_queue_reportHandler_subscriptionEstablished mtrBaseDevice  endpointID clusterID attributeID params queue reportHandler subscriptionEstablished =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr clusterID $ \raw_clusterID ->
      withObjCPtr attributeID $ \raw_attributeID ->
        withObjCPtr params $ \raw_params ->
          withObjCPtr queue $ \raw_queue ->
              sendMsg mtrBaseDevice (mkSelector "subscribeToAttributesWithEndpointID:clusterID:attributeID:params:queue:reportHandler:subscriptionEstablished:") retVoid [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_attributeID :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ())]

-- | Subscribes to multiple attribute or event paths.
--
-- Nil is treated as an empty array for attributePaths and eventPaths.
--
-- Lists of attribute and event paths to subscribe to can be provided via attributePaths and eventPaths.
--
-- The reportHandler will be called with an error if the subscription fails entirely (including when both attributePaths and eventPaths are empty).
--
-- The reportHandler will be called with arrays of response-value dictionaries (which may be data or errors) as path-specific data is received.
--
-- subscriptionEstablished will be called when the subscription is first successfully established (after the initial set of data reports has been delivered to reportHandler).  If params allow automatic resubscription, it will be called any time resubscription succeeds.
--
-- resubscriptionScheduled will be called if subscription drop is detected and params allow automatic resubscription.
--
-- If the sum of the lengths of attributePaths and eventPaths exceeds 3, the subscribe may fail due to the device not supporting that many paths for a subscription.
--
-- ObjC selector: @- subscribeToAttributePaths:eventPaths:params:queue:reportHandler:subscriptionEstablished:resubscriptionScheduled:@
subscribeToAttributePaths_eventPaths_params_queue_reportHandler_subscriptionEstablished_resubscriptionScheduled :: (IsMTRBaseDevice mtrBaseDevice, IsNSArray attributePaths, IsNSArray eventPaths, IsMTRSubscribeParams params, IsNSObject queue) => mtrBaseDevice -> attributePaths -> eventPaths -> params -> queue -> Ptr () -> Ptr () -> Ptr () -> IO ()
subscribeToAttributePaths_eventPaths_params_queue_reportHandler_subscriptionEstablished_resubscriptionScheduled mtrBaseDevice  attributePaths eventPaths params queue reportHandler subscriptionEstablished resubscriptionScheduled =
  withObjCPtr attributePaths $ \raw_attributePaths ->
    withObjCPtr eventPaths $ \raw_eventPaths ->
      withObjCPtr params $ \raw_params ->
        withObjCPtr queue $ \raw_queue ->
            sendMsg mtrBaseDevice (mkSelector "subscribeToAttributePaths:eventPaths:params:queue:reportHandler:subscriptionEstablished:resubscriptionScheduled:") retVoid [argPtr (castPtr raw_attributePaths :: Ptr ()), argPtr (castPtr raw_eventPaths :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr resubscriptionScheduled :: Ptr ())]

-- | Deregister all local report handlers for a remote device
--
-- This method is applicable only for a remote device. For a local device, the stack has to be shutdown to stop report handlers. There could be multiple clients accessing a node through a remote controller object and hence it is not appropriate for one of those clients to shut down the entire stack to stop receiving reports.
--
-- ObjC selector: @- deregisterReportHandlersWithQueue:completion:@
deregisterReportHandlersWithQueue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSObject queue) => mtrBaseDevice -> queue -> Ptr () -> IO ()
deregisterReportHandlersWithQueue_completion mtrBaseDevice  queue completion =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrBaseDevice (mkSelector "deregisterReportHandlersWithQueue:completion:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Open a commissioning window on the device.
--
-- On success, completion will be called on queue with the MTRSetupPayload that can be used to commission the device.
--
-- @setupPasscode@ — The setup passcode to use for the commissioning window.                      See MTRSetupPayload's generateRandomSetupPasscode for                      generating a valid random passcode.
--
-- @discriminator@ — The discriminator to use for the commissionable                      advertisement.
--
-- @duration@ — Duration, in seconds, during which the commissioning                      window will be open.
--
-- ObjC selector: @- openCommissioningWindowWithSetupPasscode:discriminator:duration:queue:completion:@
openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber setupPasscode, IsNSNumber discriminator, IsNSNumber duration, IsNSObject queue) => mtrBaseDevice -> setupPasscode -> discriminator -> duration -> queue -> Ptr () -> IO ()
openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completion mtrBaseDevice  setupPasscode discriminator duration queue completion =
  withObjCPtr setupPasscode $ \raw_setupPasscode ->
    withObjCPtr discriminator $ \raw_discriminator ->
      withObjCPtr duration $ \raw_duration ->
        withObjCPtr queue $ \raw_queue ->
            sendMsg mtrBaseDevice (mkSelector "openCommissioningWindowWithSetupPasscode:discriminator:duration:queue:completion:") retVoid [argPtr (castPtr raw_setupPasscode :: Ptr ()), argPtr (castPtr raw_discriminator :: Ptr ()), argPtr (castPtr raw_duration :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Open a commissioning window on the device, using a random setup passcode.
--
-- On success, completion will be called on queue with the MTRSetupPayload that can be used to commission the device.
--
-- @discriminator@ — The discriminator to use for the commissionable                      advertisement.
--
-- @duration@ — Duration, in seconds, during which the commissioning                      window will be open.
--
-- ObjC selector: @- openCommissioningWindowWithDiscriminator:duration:queue:completion:@
openCommissioningWindowWithDiscriminator_duration_queue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber discriminator, IsNSNumber duration, IsNSObject queue) => mtrBaseDevice -> discriminator -> duration -> queue -> Ptr () -> IO ()
openCommissioningWindowWithDiscriminator_duration_queue_completion mtrBaseDevice  discriminator duration queue completion =
  withObjCPtr discriminator $ \raw_discriminator ->
    withObjCPtr duration $ \raw_duration ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseDevice (mkSelector "openCommissioningWindowWithDiscriminator:duration:queue:completion:") retVoid [argPtr (castPtr raw_discriminator :: Ptr ()), argPtr (castPtr raw_duration :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Reads events from the device.
--
-- Nil values for endpointID, clusterID, eventID indicate wildcards (e.g. nil eventID means "read all the events from the endpoint(s) and cluster(s) that match endpointID/clusterID").
--
-- If all of endpointID, clusterID, eventID are non-nil, all the matching instances of a single event will be read.
--
-- If all of endpointID, clusterID, eventID are nil, all events on the device will be read.
--
-- The completion will be called with an error if the entire read interaction fails. Otherwise it will be called with values, which may be empty (e.g. if no paths matched the wildcard) or may include per-path errors if particular paths failed.
--
-- ObjC selector: @- readEventsWithEndpointID:clusterID:eventID:params:queue:completion:@
readEventsWithEndpointID_clusterID_eventID_params_queue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber eventID, IsMTRReadParams params, IsNSObject queue) => mtrBaseDevice -> endpointID -> clusterID -> eventID -> params -> queue -> Ptr () -> IO ()
readEventsWithEndpointID_clusterID_eventID_params_queue_completion mtrBaseDevice  endpointID clusterID eventID params queue completion =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr clusterID $ \raw_clusterID ->
      withObjCPtr eventID $ \raw_eventID ->
        withObjCPtr params $ \raw_params ->
          withObjCPtr queue $ \raw_queue ->
              sendMsg mtrBaseDevice (mkSelector "readEventsWithEndpointID:clusterID:eventID:params:queue:completion:") retVoid [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_eventID :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Subscribes to the specified events on the device.
--
-- Nil values for endpointID, clusterID, eventID indicate wildcards (e.g. nil eventID means "subscribe to all the events from the endpoint(s) and cluster(s) that match endpointID/clusterID").
--
-- If all of endpointID, clusterID, eventID are non-nil, a single event will be subscribed to.
--
-- If all of endpointID, clusterID, eventID are nil, all events on the device will be subscribed to.
--
-- The reportHandler will be called with an error if the subscription fails entirely.
--
-- The reportHandler will be called with arrays of response-value dictionaries (which may be data or errors) as path-specific data is received.
--
-- subscriptionEstablished will be called when the subscription is first successfully established (after the initial set of data reports has been delivered to reportHandler).  If params allow automatic resubscription, it will be called any time resubscription succeeds.
--
-- ObjC selector: @- subscribeToEventsWithEndpointID:clusterID:eventID:params:queue:reportHandler:subscriptionEstablished:@
subscribeToEventsWithEndpointID_clusterID_eventID_params_queue_reportHandler_subscriptionEstablished :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber eventID, IsMTRSubscribeParams params, IsNSObject queue) => mtrBaseDevice -> endpointID -> clusterID -> eventID -> params -> queue -> Ptr () -> Ptr () -> IO ()
subscribeToEventsWithEndpointID_clusterID_eventID_params_queue_reportHandler_subscriptionEstablished mtrBaseDevice  endpointID clusterID eventID params queue reportHandler subscriptionEstablished =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr clusterID $ \raw_clusterID ->
      withObjCPtr eventID $ \raw_eventID ->
        withObjCPtr params $ \raw_params ->
          withObjCPtr queue $ \raw_queue ->
              sendMsg mtrBaseDevice (mkSelector "subscribeToEventsWithEndpointID:clusterID:eventID:params:queue:reportHandler:subscriptionEstablished:") retVoid [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_eventID :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ())]

-- | Download log of the desired type from the device.
--
-- Note: The consumer of this API should move the file that the url points to or open it for reading before the completion handler returns. Otherwise, the file will be deleted, and the data will be lost.
--
-- @type@ — The type of log being requested. This should correspond to a value in the enum MTRDiagnosticLogType.
--
-- @timeout@ — The timeout for getting the log. If the timeout expires, completion will be called with whatever                   has been retrieved by that point (which might be none or a partial log).                   If the timeout is set to 0, the request will not expire and completion will not be called until                   the log is fully retrieved or an error occurs.
--
-- @queue@ — The queue on which completion will be called.
--
-- @completion@ — The completion handler that is called after attempting to retrieve the requested log.                     - In case of success, the completion handler is called with a non-nil URL and a nil error.                     - If there is an error, a non-nil error is used and the url can be non-nil too if some logs have already been downloaded.
--
-- ObjC selector: @- downloadLogOfType:timeout:queue:completion:@
downloadLogOfType_timeout_queue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSObject queue) => mtrBaseDevice -> MTRDiagnosticLogType -> CDouble -> queue -> Ptr () -> IO ()
downloadLogOfType_timeout_queue_completion mtrBaseDevice  type_ timeout queue completion =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrBaseDevice (mkSelector "downloadLogOfType:timeout:queue:completion:") retVoid [argCLong (coerce type_), argCDouble timeout, argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Deprecated MTRBaseDevice APIs.
--
-- ObjC selector: @- subscribeWithQueue:minInterval:maxInterval:params:cacheContainer:attributeReportHandler:eventReportHandler:errorHandler:subscriptionEstablished:resubscriptionScheduled:@
subscribeWithQueue_minInterval_maxInterval_params_cacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduled :: (IsMTRBaseDevice mtrBaseDevice, IsNSObject queue, IsMTRSubscribeParams params, IsMTRAttributeCacheContainer attributeCacheContainer) => mtrBaseDevice -> queue -> CUShort -> CUShort -> params -> attributeCacheContainer -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO ()
subscribeWithQueue_minInterval_maxInterval_params_cacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduled mtrBaseDevice  queue minInterval maxInterval params attributeCacheContainer attributeReportHandler eventReportHandler errorHandler subscriptionEstablishedHandler resubscriptionScheduledHandler =
  withObjCPtr queue $ \raw_queue ->
    withObjCPtr params $ \raw_params ->
      withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
          sendMsg mtrBaseDevice (mkSelector "subscribeWithQueue:minInterval:maxInterval:params:cacheContainer:attributeReportHandler:eventReportHandler:errorHandler:subscriptionEstablished:resubscriptionScheduled:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argCUInt (fromIntegral minInterval), argCUInt (fromIntegral maxInterval), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr attributeReportHandler :: Ptr ()), argPtr (castPtr eventReportHandler :: Ptr ()), argPtr (castPtr errorHandler :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr resubscriptionScheduledHandler :: Ptr ())]

-- | @- readAttributeWithEndpointId:clusterId:attributeId:params:clientQueue:completion:@
readAttributeWithEndpointId_clusterId_attributeId_params_clientQueue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber attributeId, IsMTRReadParams params, IsNSObject clientQueue) => mtrBaseDevice -> endpointId -> clusterId -> attributeId -> params -> clientQueue -> Ptr () -> IO ()
readAttributeWithEndpointId_clusterId_attributeId_params_clientQueue_completion mtrBaseDevice  endpointId clusterId attributeId params clientQueue completion =
  withObjCPtr endpointId $ \raw_endpointId ->
    withObjCPtr clusterId $ \raw_clusterId ->
      withObjCPtr attributeId $ \raw_attributeId ->
        withObjCPtr params $ \raw_params ->
          withObjCPtr clientQueue $ \raw_clientQueue ->
              sendMsg mtrBaseDevice (mkSelector "readAttributeWithEndpointId:clusterId:attributeId:params:clientQueue:completion:") retVoid [argPtr (castPtr raw_endpointId :: Ptr ()), argPtr (castPtr raw_clusterId :: Ptr ()), argPtr (castPtr raw_attributeId :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_clientQueue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeWithEndpointId:clusterId:attributeId:value:timedWriteTimeout:clientQueue:completion:@
writeAttributeWithEndpointId_clusterId_attributeId_value_timedWriteTimeout_clientQueue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber attributeId, IsNSNumber timeoutMs, IsNSObject clientQueue) => mtrBaseDevice -> endpointId -> clusterId -> attributeId -> RawId -> timeoutMs -> clientQueue -> Ptr () -> IO ()
writeAttributeWithEndpointId_clusterId_attributeId_value_timedWriteTimeout_clientQueue_completion mtrBaseDevice  endpointId clusterId attributeId value timeoutMs clientQueue completion =
  withObjCPtr endpointId $ \raw_endpointId ->
    withObjCPtr clusterId $ \raw_clusterId ->
      withObjCPtr attributeId $ \raw_attributeId ->
        withObjCPtr timeoutMs $ \raw_timeoutMs ->
          withObjCPtr clientQueue $ \raw_clientQueue ->
              sendMsg mtrBaseDevice (mkSelector "writeAttributeWithEndpointId:clusterId:attributeId:value:timedWriteTimeout:clientQueue:completion:") retVoid [argPtr (castPtr raw_endpointId :: Ptr ()), argPtr (castPtr raw_clusterId :: Ptr ()), argPtr (castPtr raw_attributeId :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_timeoutMs :: Ptr ()), argPtr (castPtr raw_clientQueue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- invokeCommandWithEndpointId:clusterId:commandId:commandFields:timedInvokeTimeout:clientQueue:completion:@
invokeCommandWithEndpointId_clusterId_commandId_commandFields_timedInvokeTimeout_clientQueue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber commandId, IsNSNumber timeoutMs, IsNSObject clientQueue) => mtrBaseDevice -> endpointId -> clusterId -> commandId -> RawId -> timeoutMs -> clientQueue -> Ptr () -> IO ()
invokeCommandWithEndpointId_clusterId_commandId_commandFields_timedInvokeTimeout_clientQueue_completion mtrBaseDevice  endpointId clusterId commandId commandFields timeoutMs clientQueue completion =
  withObjCPtr endpointId $ \raw_endpointId ->
    withObjCPtr clusterId $ \raw_clusterId ->
      withObjCPtr commandId $ \raw_commandId ->
        withObjCPtr timeoutMs $ \raw_timeoutMs ->
          withObjCPtr clientQueue $ \raw_clientQueue ->
              sendMsg mtrBaseDevice (mkSelector "invokeCommandWithEndpointId:clusterId:commandId:commandFields:timedInvokeTimeout:clientQueue:completion:") retVoid [argPtr (castPtr raw_endpointId :: Ptr ()), argPtr (castPtr raw_clusterId :: Ptr ()), argPtr (castPtr raw_commandId :: Ptr ()), argPtr (castPtr (unRawId commandFields) :: Ptr ()), argPtr (castPtr raw_timeoutMs :: Ptr ()), argPtr (castPtr raw_clientQueue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeWithEndpointId:clusterId:attributeId:minInterval:maxInterval:params:clientQueue:reportHandler:subscriptionEstablished:@
subscribeAttributeWithEndpointId_clusterId_attributeId_minInterval_maxInterval_params_clientQueue_reportHandler_subscriptionEstablished :: (IsMTRBaseDevice mtrBaseDevice, IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber attributeId, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params, IsNSObject clientQueue) => mtrBaseDevice -> endpointId -> clusterId -> attributeId -> minInterval -> maxInterval -> params -> clientQueue -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWithEndpointId_clusterId_attributeId_minInterval_maxInterval_params_clientQueue_reportHandler_subscriptionEstablished mtrBaseDevice  endpointId clusterId attributeId minInterval maxInterval params clientQueue reportHandler subscriptionEstablishedHandler =
  withObjCPtr endpointId $ \raw_endpointId ->
    withObjCPtr clusterId $ \raw_clusterId ->
      withObjCPtr attributeId $ \raw_attributeId ->
        withObjCPtr minInterval $ \raw_minInterval ->
          withObjCPtr maxInterval $ \raw_maxInterval ->
            withObjCPtr params $ \raw_params ->
              withObjCPtr clientQueue $ \raw_clientQueue ->
                  sendMsg mtrBaseDevice (mkSelector "subscribeAttributeWithEndpointId:clusterId:attributeId:minInterval:maxInterval:params:clientQueue:reportHandler:subscriptionEstablished:") retVoid [argPtr (castPtr raw_endpointId :: Ptr ()), argPtr (castPtr raw_clusterId :: Ptr ()), argPtr (castPtr raw_attributeId :: Ptr ()), argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_clientQueue :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ())]

-- | @- deregisterReportHandlersWithClientQueue:completion:@
deregisterReportHandlersWithClientQueue_completion :: (IsMTRBaseDevice mtrBaseDevice, IsNSObject queue) => mtrBaseDevice -> queue -> Ptr () -> IO ()
deregisterReportHandlersWithClientQueue_completion mtrBaseDevice  queue completion =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrBaseDevice (mkSelector "deregisterReportHandlersWithClientQueue:completion:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | The transport used by the current session with this device, or @MTRTransportTypeUndefined@ if no session is currently active.
--
-- ObjC selector: @- sessionTransportType@
sessionTransportType :: IsMTRBaseDevice mtrBaseDevice => mtrBaseDevice -> IO MTRTransportType
sessionTransportType mtrBaseDevice  =
    fmap (coerce :: CUChar -> MTRTransportType) $ sendMsg mtrBaseDevice (mkSelector "sessionTransportType") retCUChar []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @deviceWithNodeID:controller:@
deviceWithNodeID_controllerSelector :: Selector
deviceWithNodeID_controllerSelector = mkSelector "deviceWithNodeID:controller:"

-- | @Selector@ for @subscribeWithQueue:params:clusterStateCacheContainer:attributeReportHandler:eventReportHandler:errorHandler:subscriptionEstablished:resubscriptionScheduled:@
subscribeWithQueue_params_clusterStateCacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduledSelector :: Selector
subscribeWithQueue_params_clusterStateCacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduledSelector = mkSelector "subscribeWithQueue:params:clusterStateCacheContainer:attributeReportHandler:eventReportHandler:errorHandler:subscriptionEstablished:resubscriptionScheduled:"

-- | @Selector@ for @readAttributesWithEndpointID:clusterID:attributeID:params:queue:completion:@
readAttributesWithEndpointID_clusterID_attributeID_params_queue_completionSelector :: Selector
readAttributesWithEndpointID_clusterID_attributeID_params_queue_completionSelector = mkSelector "readAttributesWithEndpointID:clusterID:attributeID:params:queue:completion:"

-- | @Selector@ for @readAttributePaths:eventPaths:params:queue:completion:@
readAttributePaths_eventPaths_params_queue_completionSelector :: Selector
readAttributePaths_eventPaths_params_queue_completionSelector = mkSelector "readAttributePaths:eventPaths:params:queue:completion:"

-- | @Selector@ for @writeAttributeWithEndpointID:clusterID:attributeID:value:timedWriteTimeout:queue:completion:@
writeAttributeWithEndpointID_clusterID_attributeID_value_timedWriteTimeout_queue_completionSelector :: Selector
writeAttributeWithEndpointID_clusterID_attributeID_value_timedWriteTimeout_queue_completionSelector = mkSelector "writeAttributeWithEndpointID:clusterID:attributeID:value:timedWriteTimeout:queue:completion:"

-- | @Selector@ for @invokeCommandWithEndpointID:clusterID:commandID:commandFields:timedInvokeTimeout:queue:completion:@
invokeCommandWithEndpointID_clusterID_commandID_commandFields_timedInvokeTimeout_queue_completionSelector :: Selector
invokeCommandWithEndpointID_clusterID_commandID_commandFields_timedInvokeTimeout_queue_completionSelector = mkSelector "invokeCommandWithEndpointID:clusterID:commandID:commandFields:timedInvokeTimeout:queue:completion:"

-- | @Selector@ for @subscribeToAttributesWithEndpointID:clusterID:attributeID:params:queue:reportHandler:subscriptionEstablished:@
subscribeToAttributesWithEndpointID_clusterID_attributeID_params_queue_reportHandler_subscriptionEstablishedSelector :: Selector
subscribeToAttributesWithEndpointID_clusterID_attributeID_params_queue_reportHandler_subscriptionEstablishedSelector = mkSelector "subscribeToAttributesWithEndpointID:clusterID:attributeID:params:queue:reportHandler:subscriptionEstablished:"

-- | @Selector@ for @subscribeToAttributePaths:eventPaths:params:queue:reportHandler:subscriptionEstablished:resubscriptionScheduled:@
subscribeToAttributePaths_eventPaths_params_queue_reportHandler_subscriptionEstablished_resubscriptionScheduledSelector :: Selector
subscribeToAttributePaths_eventPaths_params_queue_reportHandler_subscriptionEstablished_resubscriptionScheduledSelector = mkSelector "subscribeToAttributePaths:eventPaths:params:queue:reportHandler:subscriptionEstablished:resubscriptionScheduled:"

-- | @Selector@ for @deregisterReportHandlersWithQueue:completion:@
deregisterReportHandlersWithQueue_completionSelector :: Selector
deregisterReportHandlersWithQueue_completionSelector = mkSelector "deregisterReportHandlersWithQueue:completion:"

-- | @Selector@ for @openCommissioningWindowWithSetupPasscode:discriminator:duration:queue:completion:@
openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completionSelector :: Selector
openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completionSelector = mkSelector "openCommissioningWindowWithSetupPasscode:discriminator:duration:queue:completion:"

-- | @Selector@ for @openCommissioningWindowWithDiscriminator:duration:queue:completion:@
openCommissioningWindowWithDiscriminator_duration_queue_completionSelector :: Selector
openCommissioningWindowWithDiscriminator_duration_queue_completionSelector = mkSelector "openCommissioningWindowWithDiscriminator:duration:queue:completion:"

-- | @Selector@ for @readEventsWithEndpointID:clusterID:eventID:params:queue:completion:@
readEventsWithEndpointID_clusterID_eventID_params_queue_completionSelector :: Selector
readEventsWithEndpointID_clusterID_eventID_params_queue_completionSelector = mkSelector "readEventsWithEndpointID:clusterID:eventID:params:queue:completion:"

-- | @Selector@ for @subscribeToEventsWithEndpointID:clusterID:eventID:params:queue:reportHandler:subscriptionEstablished:@
subscribeToEventsWithEndpointID_clusterID_eventID_params_queue_reportHandler_subscriptionEstablishedSelector :: Selector
subscribeToEventsWithEndpointID_clusterID_eventID_params_queue_reportHandler_subscriptionEstablishedSelector = mkSelector "subscribeToEventsWithEndpointID:clusterID:eventID:params:queue:reportHandler:subscriptionEstablished:"

-- | @Selector@ for @downloadLogOfType:timeout:queue:completion:@
downloadLogOfType_timeout_queue_completionSelector :: Selector
downloadLogOfType_timeout_queue_completionSelector = mkSelector "downloadLogOfType:timeout:queue:completion:"

-- | @Selector@ for @subscribeWithQueue:minInterval:maxInterval:params:cacheContainer:attributeReportHandler:eventReportHandler:errorHandler:subscriptionEstablished:resubscriptionScheduled:@
subscribeWithQueue_minInterval_maxInterval_params_cacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduledSelector :: Selector
subscribeWithQueue_minInterval_maxInterval_params_cacheContainer_attributeReportHandler_eventReportHandler_errorHandler_subscriptionEstablished_resubscriptionScheduledSelector = mkSelector "subscribeWithQueue:minInterval:maxInterval:params:cacheContainer:attributeReportHandler:eventReportHandler:errorHandler:subscriptionEstablished:resubscriptionScheduled:"

-- | @Selector@ for @readAttributeWithEndpointId:clusterId:attributeId:params:clientQueue:completion:@
readAttributeWithEndpointId_clusterId_attributeId_params_clientQueue_completionSelector :: Selector
readAttributeWithEndpointId_clusterId_attributeId_params_clientQueue_completionSelector = mkSelector "readAttributeWithEndpointId:clusterId:attributeId:params:clientQueue:completion:"

-- | @Selector@ for @writeAttributeWithEndpointId:clusterId:attributeId:value:timedWriteTimeout:clientQueue:completion:@
writeAttributeWithEndpointId_clusterId_attributeId_value_timedWriteTimeout_clientQueue_completionSelector :: Selector
writeAttributeWithEndpointId_clusterId_attributeId_value_timedWriteTimeout_clientQueue_completionSelector = mkSelector "writeAttributeWithEndpointId:clusterId:attributeId:value:timedWriteTimeout:clientQueue:completion:"

-- | @Selector@ for @invokeCommandWithEndpointId:clusterId:commandId:commandFields:timedInvokeTimeout:clientQueue:completion:@
invokeCommandWithEndpointId_clusterId_commandId_commandFields_timedInvokeTimeout_clientQueue_completionSelector :: Selector
invokeCommandWithEndpointId_clusterId_commandId_commandFields_timedInvokeTimeout_clientQueue_completionSelector = mkSelector "invokeCommandWithEndpointId:clusterId:commandId:commandFields:timedInvokeTimeout:clientQueue:completion:"

-- | @Selector@ for @subscribeAttributeWithEndpointId:clusterId:attributeId:minInterval:maxInterval:params:clientQueue:reportHandler:subscriptionEstablished:@
subscribeAttributeWithEndpointId_clusterId_attributeId_minInterval_maxInterval_params_clientQueue_reportHandler_subscriptionEstablishedSelector :: Selector
subscribeAttributeWithEndpointId_clusterId_attributeId_minInterval_maxInterval_params_clientQueue_reportHandler_subscriptionEstablishedSelector = mkSelector "subscribeAttributeWithEndpointId:clusterId:attributeId:minInterval:maxInterval:params:clientQueue:reportHandler:subscriptionEstablished:"

-- | @Selector@ for @deregisterReportHandlersWithClientQueue:completion:@
deregisterReportHandlersWithClientQueue_completionSelector :: Selector
deregisterReportHandlersWithClientQueue_completionSelector = mkSelector "deregisterReportHandlersWithClientQueue:completion:"

-- | @Selector@ for @sessionTransportType@
sessionTransportTypeSelector :: Selector
sessionTransportTypeSelector = mkSelector "sessionTransportType"

