{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceController@.
module ObjC.Matter.MTRDeviceController
  ( MTRDeviceController
  , IsMTRDeviceController(..)
  , init_
  , new
  , initWithParameters_error
  , setupCommissioningSessionWithPayload_newNodeID_error
  , setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_error
  , commissionNodeWithID_commissioningParams_error
  , continueCommissioningDevice_ignoreAttestationFailure_error
  , cancelCommissioningForNodeID_error
  , deviceBeingCommissionedWithNodeID_error
  , preWarmCommissioningSession
  , setDeviceControllerDelegate_queue
  , addDeviceControllerDelegate_queue
  , removeDeviceControllerDelegate
  , startBrowseForCommissionables_queue
  , stopBrowseForCommissionables
  , attestationChallengeForDeviceID
  , addServerEndpoint
  , removeServerEndpoint_queue_completion
  , removeServerEndpoint
  , forgetDeviceWithNodeID
  , computePASEVerifierForSetupPasscode_iterations_salt_error
  , suspend
  , resume
  , shutdown
  , sharedControllerWithId_xpcConnectBlock
  , sharedControllerWithID_xpcConnectBlock
  , encodeXPCResponseValues
  , decodeXPCResponseValues
  , encodeXPCReadParams
  , decodeXPCReadParams
  , encodeXPCSubscribeParams
  , decodeXPCSubscribeParams
  , xpcInterfaceForServerProtocol
  , xpcInterfaceForClientProtocol
  , fetchAttestationChallengeForDeviceId
  , getBaseDevice_queue_completionHandler
  , pairDevice_discriminator_setupPINCode_error
  , pairDevice_address_port_setupPINCode_error
  , pairDevice_onboardingPayload_error
  , commissionDevice_commissioningParams_error
  , stopDevicePairing_error
  , getDeviceBeingCommissioned_error
  , openPairingWindow_duration_error
  , openPairingWindowWithPIN_duration_discriminator_setupPIN_error
  , computePaseVerifier_iterations_salt
  , setPairingDelegate_queue
  , setNocChainIssuer_queue
  , running
  , suspended
  , uniqueIdentifier
  , controllerNodeID
  , devices
  , nodesWithStoredData
  , controllerNodeId
  , initSelector
  , newSelector
  , initWithParameters_errorSelector
  , setupCommissioningSessionWithPayload_newNodeID_errorSelector
  , setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_errorSelector
  , commissionNodeWithID_commissioningParams_errorSelector
  , continueCommissioningDevice_ignoreAttestationFailure_errorSelector
  , cancelCommissioningForNodeID_errorSelector
  , deviceBeingCommissionedWithNodeID_errorSelector
  , preWarmCommissioningSessionSelector
  , setDeviceControllerDelegate_queueSelector
  , addDeviceControllerDelegate_queueSelector
  , removeDeviceControllerDelegateSelector
  , startBrowseForCommissionables_queueSelector
  , stopBrowseForCommissionablesSelector
  , attestationChallengeForDeviceIDSelector
  , addServerEndpointSelector
  , removeServerEndpoint_queue_completionSelector
  , removeServerEndpointSelector
  , forgetDeviceWithNodeIDSelector
  , computePASEVerifierForSetupPasscode_iterations_salt_errorSelector
  , suspendSelector
  , resumeSelector
  , shutdownSelector
  , sharedControllerWithId_xpcConnectBlockSelector
  , sharedControllerWithID_xpcConnectBlockSelector
  , encodeXPCResponseValuesSelector
  , decodeXPCResponseValuesSelector
  , encodeXPCReadParamsSelector
  , decodeXPCReadParamsSelector
  , encodeXPCSubscribeParamsSelector
  , decodeXPCSubscribeParamsSelector
  , xpcInterfaceForServerProtocolSelector
  , xpcInterfaceForClientProtocolSelector
  , fetchAttestationChallengeForDeviceIdSelector
  , getBaseDevice_queue_completionHandlerSelector
  , pairDevice_discriminator_setupPINCode_errorSelector
  , pairDevice_address_port_setupPINCode_errorSelector
  , pairDevice_onboardingPayload_errorSelector
  , commissionDevice_commissioningParams_errorSelector
  , stopDevicePairing_errorSelector
  , getDeviceBeingCommissioned_errorSelector
  , openPairingWindow_duration_errorSelector
  , openPairingWindowWithPIN_duration_discriminator_setupPIN_errorSelector
  , computePaseVerifier_iterations_saltSelector
  , setPairingDelegate_queueSelector
  , setNocChainIssuer_queueSelector
  , runningSelector
  , suspendedSelector
  , uniqueIdentifierSelector
  , controllerNodeIDSelector
  , devicesSelector
  , nodesWithStoredDataSelector
  , controllerNodeIdSelector


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

-- | Controllers are created via the MTRDeviceControllerFactory object or initialized via initWithParameters:error:.
--
-- ObjC selector: @- init@
init_ :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id MTRDeviceController)
init_ mtrDeviceController  =
    sendMsg mtrDeviceController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRDeviceController)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize a device controller with the provided parameters.  This will:
--
-- 1) Auto-start the MTRDeviceControllerFactory in storage-per-controller mode    if it has not already been started. 2) Return nil or a running controller.
--
-- Once this returns non-nil, it's the caller's responsibility to call shutdown on the controller to avoid leaking it.
--
-- ObjC selector: @- initWithParameters:error:@
initWithParameters_error :: (IsMTRDeviceController mtrDeviceController, IsMTRDeviceControllerAbstractParameters parameters, IsNSError error_) => mtrDeviceController -> parameters -> error_ -> IO (Id MTRDeviceController)
initWithParameters_error mtrDeviceController  parameters error_ =
  withObjCPtr parameters $ \raw_parameters ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDeviceController (mkSelector "initWithParameters:error:") (retPtr retVoid) [argPtr (castPtr raw_parameters :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Set up a commissioning session for a device, using the provided setup payload to discover it and connect to it.
--
-- @payload@ — a setup payload (probably created from a QR code or numeric                code onboarding payload).
--
-- @newNodeID@ — the planned node id for the node.  error indication if discovery can't start at all (e.g. because the              setup payload is invalid).
--
-- The IP and port for the device will be discovered automatically based on the provided discriminator.
--
-- Then a PASE session will be established with the device, unless an error occurs.  MTRDeviceControllerDelegate will be notified as follows:
--
-- * Discovery fails: controller:statusUpdate: with MTRCommissioningStatusFailed.
--
-- * Commissioning session setup fails:   controller:commissioningSessionEstablishmentDone: with non-nil error.
--
-- * Commissioning session setup succeeds:   controller:commissioningSessionEstablishmentDone: with nil error.
--
-- Once a commissioning session is set up, getDeviceBeingCommissioned can be used to get an MTRBaseDevice and discover what sort of network credentials the device might need, and commissionDevice can be used to commission the device.
--
-- ObjC selector: @- setupCommissioningSessionWithPayload:newNodeID:error:@
setupCommissioningSessionWithPayload_newNodeID_error :: (IsMTRDeviceController mtrDeviceController, IsMTRSetupPayload payload, IsNSNumber newNodeID, IsNSError error_) => mtrDeviceController -> payload -> newNodeID -> error_ -> IO Bool
setupCommissioningSessionWithPayload_newNodeID_error mtrDeviceController  payload newNodeID error_ =
  withObjCPtr payload $ \raw_payload ->
    withObjCPtr newNodeID $ \raw_newNodeID ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "setupCommissioningSessionWithPayload:newNodeID:error:") retCULong [argPtr (castPtr raw_payload :: Ptr ()), argPtr (castPtr raw_newNodeID :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Set up a commissioning session for a device, using the provided discovered result to connect to it.
--
-- @discoveredDevice@ — a previously discovered device.
--
-- @payload@ — a setup payload (probably created from a QR code or numeric                code onboarding payload).
--
-- @newNodeID@ — the planned node id for the node.  error indication if the commissioning session establishment can't start at all.
--
-- The connection information for the device will be retrieved from the discovered device. A device discovered over DNS-SD will use the discovered IPs/ports, while a device discovered over BLE will use the underlying CBPeripheral.
--
-- Then a PASE session will be established with the device, unless an error occurs.  MTRDeviceControllerDelegate will be notified as follows:
--
-- * Invalid connection information: controller:statusUpdate: with MTRCommissioningStatusFailed.
--
-- * Commissioning session setup fails:   controller:commissioningSessionEstablishmentDone: with non-nil error.
--
-- * Commissioning session setup succeeds:   controller:commissioningSessionEstablishmentDone: with nil error.
--
-- Once a commissioning session is set up, getDeviceBeingCommissioned can be used to get an MTRBaseDevice and discover what sort of network credentials the device might need, and commissionDevice can be used to commission the device.
--
-- ObjC selector: @- setupCommissioningSessionWithDiscoveredDevice:payload:newNodeID:error:@
setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_error :: (IsMTRDeviceController mtrDeviceController, IsMTRCommissionableBrowserResult discoveredDevice, IsMTRSetupPayload payload, IsNSNumber newNodeID, IsNSError error_) => mtrDeviceController -> discoveredDevice -> payload -> newNodeID -> error_ -> IO Bool
setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_error mtrDeviceController  discoveredDevice payload newNodeID error_ =
  withObjCPtr discoveredDevice $ \raw_discoveredDevice ->
    withObjCPtr payload $ \raw_payload ->
      withObjCPtr newNodeID $ \raw_newNodeID ->
        withObjCPtr error_ $ \raw_error_ ->
            fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "setupCommissioningSessionWithDiscoveredDevice:payload:newNodeID:error:") retCULong [argPtr (castPtr raw_discoveredDevice :: Ptr ()), argPtr (castPtr raw_payload :: Ptr ()), argPtr (castPtr raw_newNodeID :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Commission the node with the given node ID.  The node ID must match the node ID that was used to set up the commissioning session.
--
-- NOTE: The forceWiFiScan and forceThreadScan properties of MTRCommissioningParameters are ignored by this API.
--
-- ObjC selector: @- commissionNodeWithID:commissioningParams:error:@
commissionNodeWithID_commissioningParams_error :: (IsMTRDeviceController mtrDeviceController, IsNSNumber nodeID, IsMTRCommissioningParameters commissioningParams, IsNSError error_) => mtrDeviceController -> nodeID -> commissioningParams -> error_ -> IO Bool
commissionNodeWithID_commissioningParams_error mtrDeviceController  nodeID commissioningParams error_ =
  withObjCPtr nodeID $ \raw_nodeID ->
    withObjCPtr commissioningParams $ \raw_commissioningParams ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "commissionNodeWithID:commissioningParams:error:") retCULong [argPtr (castPtr raw_nodeID :: Ptr ()), argPtr (castPtr raw_commissioningParams :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Call this method after MTRDeviceAttestationDelegate deviceAttestationFailedForController:opaqueDeviceHandle:error: or deviceAttestationCompletedForController:opaqueDeviceHandle:attestationDeviceInfo:error: is called to continue commissioning the device.
--
-- ObjC selector: @- continueCommissioningDevice:ignoreAttestationFailure:error:@
continueCommissioningDevice_ignoreAttestationFailure_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> Ptr () -> Bool -> error_ -> IO Bool
continueCommissioningDevice_ignoreAttestationFailure_error mtrDeviceController  opaqueDeviceHandle ignoreAttestationFailure error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "continueCommissioningDevice:ignoreAttestationFailure:error:") retCULong [argPtr opaqueDeviceHandle, argCULong (if ignoreAttestationFailure then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Cancel commissioning for the given node id.  This will shut down any existing commissioning session for that node id.
--
-- ObjC selector: @- cancelCommissioningForNodeID:error:@
cancelCommissioningForNodeID_error :: (IsMTRDeviceController mtrDeviceController, IsNSNumber nodeID, IsNSError error_) => mtrDeviceController -> nodeID -> error_ -> IO Bool
cancelCommissioningForNodeID_error mtrDeviceController  nodeID error_ =
  withObjCPtr nodeID $ \raw_nodeID ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "cancelCommissioningForNodeID:error:") retCULong [argPtr (castPtr raw_nodeID :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Get an MTRBaseDevice for a commissioning session that was set up for the given node ID.  Returns nil if no such commissioning session is available.
--
-- ObjC selector: @- deviceBeingCommissionedWithNodeID:error:@
deviceBeingCommissionedWithNodeID_error :: (IsMTRDeviceController mtrDeviceController, IsNSNumber nodeID, IsNSError error_) => mtrDeviceController -> nodeID -> error_ -> IO (Id MTRBaseDevice)
deviceBeingCommissionedWithNodeID_error mtrDeviceController  nodeID error_ =
  withObjCPtr nodeID $ \raw_nodeID ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDeviceController (mkSelector "deviceBeingCommissionedWithNodeID:error:") (retPtr retVoid) [argPtr (castPtr raw_nodeID :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- preWarmCommissioningSession@
preWarmCommissioningSession :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO ()
preWarmCommissioningSession mtrDeviceController  =
    sendMsg mtrDeviceController (mkSelector "preWarmCommissioningSession") retVoid []

-- | Set the Delegate for the device controller as well as the Queue on which the Delegate callbacks will be triggered
--
-- @delegate@ — The delegate the commissioning process should use
--
-- @queue@ — The queue on which the callbacks will be delivered
--
-- ObjC selector: @- setDeviceControllerDelegate:queue:@
setDeviceControllerDelegate_queue :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> RawId -> queue -> IO ()
setDeviceControllerDelegate_queue mtrDeviceController  delegate queue =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrDeviceController (mkSelector "setDeviceControllerDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | Adds a Delegate to the device controller as well as the Queue on which the Delegate callbacks will be triggered
--
-- Multiple delegates can be added to monitor MTRDeviceController state changes. Note that there should only be one delegate that responds to pairing related callbacks.
--
-- If a delegate is added a second time, the call would be ignored.
--
-- All delegates are held by weak references, and so if a delegate object goes away, it will be automatically removed.
--
-- @delegate@ — The delegate the commissioning process should use
--
-- @queue@ — The queue on which the callbacks will be delivered
--
-- ObjC selector: @- addDeviceControllerDelegate:queue:@
addDeviceControllerDelegate_queue :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> RawId -> queue -> IO ()
addDeviceControllerDelegate_queue mtrDeviceController  delegate queue =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrDeviceController (mkSelector "addDeviceControllerDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | Removes a Delegate from the device controller
--
-- @delegate@ — The delegate to be removed
--
-- ObjC selector: @- removeDeviceControllerDelegate:@
removeDeviceControllerDelegate :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> RawId -> IO ()
removeDeviceControllerDelegate mtrDeviceController  delegate =
    sendMsg mtrDeviceController (mkSelector "removeDeviceControllerDelegate:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ())]

-- | Start scanning for commissionable devices.
--
-- This method will fail if the controller factory is not running or the browse has already been started.
--
-- ObjC selector: @- startBrowseForCommissionables:queue:@
startBrowseForCommissionables_queue :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> RawId -> queue -> IO Bool
startBrowseForCommissionables_queue mtrDeviceController  delegate queue =
  withObjCPtr queue $ \raw_queue ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "startBrowseForCommissionables:queue:") retCULong [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | Stop scanning for commissionable devices.
--
-- This method will fail if the controller factory is not running or the browse has not been started.
--
-- ObjC selector: @- stopBrowseForCommissionables@
stopBrowseForCommissionables :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO Bool
stopBrowseForCommissionables mtrDeviceController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "stopBrowseForCommissionables") retCULong []

-- | Return the attestation challenge for the secure session of the device being commissioned.
--
-- Attempts to retrieve the attestation challenge for a commissionee with the given Device ID. Returns nil if given Device ID does not match an active commissionee, or if a Secure Session is not availale.
--
-- ObjC selector: @- attestationChallengeForDeviceID:@
attestationChallengeForDeviceID :: (IsMTRDeviceController mtrDeviceController, IsNSNumber deviceID) => mtrDeviceController -> deviceID -> IO (Id NSData)
attestationChallengeForDeviceID mtrDeviceController  deviceID =
  withObjCPtr deviceID $ \raw_deviceID ->
      sendMsg mtrDeviceController (mkSelector "attestationChallengeForDeviceID:") (retPtr retVoid) [argPtr (castPtr raw_deviceID :: Ptr ())] >>= retainedObject . castPtr

-- | Add a server endpoint for this controller.  The endpoint starts off enabled.
--
-- Will fail in the following cases:
--
-- 1) There is already an endpoint defined with the given endpoint id. 2) There are too many endpoints defined already.
--
-- ObjC selector: @- addServerEndpoint:@
addServerEndpoint :: (IsMTRDeviceController mtrDeviceController, IsMTRServerEndpoint endpoint) => mtrDeviceController -> endpoint -> IO Bool
addServerEndpoint mtrDeviceController  endpoint =
  withObjCPtr endpoint $ \raw_endpoint ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "addServerEndpoint:") retCULong [argPtr (castPtr raw_endpoint :: Ptr ())]

-- | Remove the given server endpoint from this controller.  If the endpoint is not attached to this controller, will just call the completion and do nothing else.
--
-- ObjC selector: @- removeServerEndpoint:queue:completion:@
removeServerEndpoint_queue_completion :: (IsMTRDeviceController mtrDeviceController, IsMTRServerEndpoint endpoint, IsNSObject queue) => mtrDeviceController -> endpoint -> queue -> Ptr () -> IO ()
removeServerEndpoint_queue_completion mtrDeviceController  endpoint queue completion =
  withObjCPtr endpoint $ \raw_endpoint ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrDeviceController (mkSelector "removeServerEndpoint:queue:completion:") retVoid [argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Remove the given server endpoint without being notified when the removal completes.
--
-- ObjC selector: @- removeServerEndpoint:@
removeServerEndpoint :: (IsMTRDeviceController mtrDeviceController, IsMTRServerEndpoint endpoint) => mtrDeviceController -> endpoint -> IO ()
removeServerEndpoint mtrDeviceController  endpoint =
  withObjCPtr endpoint $ \raw_endpoint ->
      sendMsg mtrDeviceController (mkSelector "removeServerEndpoint:") retVoid [argPtr (castPtr raw_endpoint :: Ptr ())]

-- | Forget any information we have about the device with the given node ID.  That includes clearing any information we have stored about it.
--
-- ObjC selector: @- forgetDeviceWithNodeID:@
forgetDeviceWithNodeID :: (IsMTRDeviceController mtrDeviceController, IsNSNumber nodeID) => mtrDeviceController -> nodeID -> IO ()
forgetDeviceWithNodeID mtrDeviceController  nodeID =
  withObjCPtr nodeID $ \raw_nodeID ->
      sendMsg mtrDeviceController (mkSelector "forgetDeviceWithNodeID:") retVoid [argPtr (castPtr raw_nodeID :: Ptr ())]

-- | Compute a PASE verifier for the desired setup passcode.
--
-- @setupPasscode@ — The desired passcode to use.
--
-- @iterations@ — The number of iterations to use when generating the verifier.
--
-- @salt@ — The 16-byte salt for verifier computation.
--
-- Returns nil on errors (e.g. salt has the wrong size), otherwise the computed verifier bytes.
--
-- ObjC selector: @+ computePASEVerifierForSetupPasscode:iterations:salt:error:@
computePASEVerifierForSetupPasscode_iterations_salt_error :: (IsNSNumber setupPasscode, IsNSNumber iterations, IsNSData salt, IsNSError error_) => setupPasscode -> iterations -> salt -> error_ -> IO (Id NSData)
computePASEVerifierForSetupPasscode_iterations_salt_error setupPasscode iterations salt error_ =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    withObjCPtr setupPasscode $ \raw_setupPasscode ->
      withObjCPtr iterations $ \raw_iterations ->
        withObjCPtr salt $ \raw_salt ->
          withObjCPtr error_ $ \raw_error_ ->
            sendClassMsg cls' (mkSelector "computePASEVerifierForSetupPasscode:iterations:salt:error:") (retPtr retVoid) [argPtr (castPtr raw_setupPasscode :: Ptr ()), argPtr (castPtr raw_iterations :: Ptr ()), argPtr (castPtr raw_salt :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Suspend the controller.  This will attempt to stop all network traffic associated with the controller.  The controller will remain suspended until it is resumed.
--
-- Suspending an already-suspended controller has no effect.
--
-- ObjC selector: @- suspend@
suspend :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO ()
suspend mtrDeviceController  =
    sendMsg mtrDeviceController (mkSelector "suspend") retVoid []

-- | Resume the controller.  This has no effect if the controller is not suspended.
--
-- A resume following any number of suspend calls will resume the controller; there does not need to be a resume call to match every suspend call.
--
-- ObjC selector: @- resume@
resume :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO ()
resume mtrDeviceController  =
    sendMsg mtrDeviceController (mkSelector "resume") retVoid []

-- | Shut down the controller. Calls to shutdown after the first one are NO-OPs. This must be called, either directly or via shutting down the MTRDeviceControllerFactory, to avoid leaking the controller.
--
-- ObjC selector: @- shutdown@
shutdown :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO ()
shutdown mtrDeviceController  =
    sendMsg mtrDeviceController (mkSelector "shutdown") retVoid []

-- | @+ sharedControllerWithId:xpcConnectBlock:@
sharedControllerWithId_xpcConnectBlock :: RawId -> Ptr () -> IO (Id MTRDeviceController)
sharedControllerWithId_xpcConnectBlock controllerID xpcConnectBlock =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMsg cls' (mkSelector "sharedControllerWithId:xpcConnectBlock:") (retPtr retVoid) [argPtr (castPtr (unRawId controllerID) :: Ptr ()), argPtr (castPtr xpcConnectBlock :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a shared device controller proxy for the controller object over XPC connection.
--
-- @controllerID@ — an implementation specific id in case multiple shared device controllers are available over XPC connection
--
-- @xpcConnectBlock@ — block to connect to an XPC listener serving the shared device controllers in an implementation specific way
--
-- ObjC selector: @+ sharedControllerWithID:xpcConnectBlock:@
sharedControllerWithID_xpcConnectBlock :: RawId -> Ptr () -> IO (Id MTRDeviceController)
sharedControllerWithID_xpcConnectBlock controllerID xpcConnectBlock =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMsg cls' (mkSelector "sharedControllerWithID:xpcConnectBlock:") (retPtr retVoid) [argPtr (castPtr (unRawId controllerID) :: Ptr ()), argPtr (castPtr xpcConnectBlock :: Ptr ())] >>= retainedObject . castPtr

-- | Returns an encoded values object to send over XPC for read, write and command interactions
--
-- ObjC selector: @+ encodeXPCResponseValues:@
encodeXPCResponseValues :: IsNSArray values => values -> IO (Id NSArray)
encodeXPCResponseValues values =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    withObjCPtr values $ \raw_values ->
      sendClassMsg cls' (mkSelector "encodeXPCResponseValues:") (retPtr retVoid) [argPtr (castPtr raw_values :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a decoded values object from a values object received from XPC for read, write and command interactions
--
-- ObjC selector: @+ decodeXPCResponseValues:@
decodeXPCResponseValues :: IsNSArray values => values -> IO (Id NSArray)
decodeXPCResponseValues values =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    withObjCPtr values $ \raw_values ->
      sendClassMsg cls' (mkSelector "decodeXPCResponseValues:") (retPtr retVoid) [argPtr (castPtr raw_values :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a serialized read parameter object to send over XPC
--
-- ObjC selector: @+ encodeXPCReadParams:@
encodeXPCReadParams :: IsMTRReadParams params => params -> IO (Id NSDictionary)
encodeXPCReadParams params =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    withObjCPtr params $ \raw_params ->
      sendClassMsg cls' (mkSelector "encodeXPCReadParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a deserialized read parameter object from an object received over XPC
--
-- ObjC selector: @+ decodeXPCReadParams:@
decodeXPCReadParams :: IsNSDictionary params => params -> IO (Id MTRReadParams)
decodeXPCReadParams params =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    withObjCPtr params $ \raw_params ->
      sendClassMsg cls' (mkSelector "decodeXPCReadParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a serialized subscribe parameter object to send over XPC
--
-- ObjC selector: @+ encodeXPCSubscribeParams:@
encodeXPCSubscribeParams :: IsMTRSubscribeParams params => params -> IO (Id NSDictionary)
encodeXPCSubscribeParams params =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    withObjCPtr params $ \raw_params ->
      sendClassMsg cls' (mkSelector "encodeXPCSubscribeParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a deserialized subscribe parameter object from an object received over XPC
--
-- ObjC selector: @+ decodeXPCSubscribeParams:@
decodeXPCSubscribeParams :: IsNSDictionary params => params -> IO (Id MTRSubscribeParams)
decodeXPCSubscribeParams params =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    withObjCPtr params $ \raw_params ->
      sendClassMsg cls' (mkSelector "decodeXPCSubscribeParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | Returns an NSXPCInterface configured for MTRDeviceControllerServerProtocol.
--
-- ObjC selector: @+ xpcInterfaceForServerProtocol@
xpcInterfaceForServerProtocol :: IO (Id NSXPCInterface)
xpcInterfaceForServerProtocol  =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMsg cls' (mkSelector "xpcInterfaceForServerProtocol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns an NSXPCInterface configured for MTRDeviceControllerClientProtocol.
--
-- ObjC selector: @+ xpcInterfaceForClientProtocol@
xpcInterfaceForClientProtocol :: IO (Id NSXPCInterface)
xpcInterfaceForClientProtocol  =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMsg cls' (mkSelector "xpcInterfaceForClientProtocol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fetchAttestationChallengeForDeviceId:@
fetchAttestationChallengeForDeviceId :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> CULong -> IO (Id NSData)
fetchAttestationChallengeForDeviceId mtrDeviceController  deviceId =
    sendMsg mtrDeviceController (mkSelector "fetchAttestationChallengeForDeviceId:") (retPtr retVoid) [argCULong deviceId] >>= retainedObject . castPtr

-- | @- getBaseDevice:queue:completionHandler:@
getBaseDevice_queue_completionHandler :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> CULong -> queue -> Ptr () -> IO Bool
getBaseDevice_queue_completionHandler mtrDeviceController  deviceID queue completionHandler =
  withObjCPtr queue $ \raw_queue ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "getBaseDevice:queue:completionHandler:") retCULong [argCULong deviceID, argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- pairDevice:discriminator:setupPINCode:error:@
pairDevice_discriminator_setupPINCode_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> CULong -> CUShort -> CUInt -> error_ -> IO Bool
pairDevice_discriminator_setupPINCode_error mtrDeviceController  deviceID discriminator setupPINCode error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "pairDevice:discriminator:setupPINCode:error:") retCULong [argCULong deviceID, argCUInt (fromIntegral discriminator), argCUInt setupPINCode, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- pairDevice:address:port:setupPINCode:error:@
pairDevice_address_port_setupPINCode_error :: (IsMTRDeviceController mtrDeviceController, IsNSString address, IsNSError error_) => mtrDeviceController -> CULong -> address -> CUShort -> CUInt -> error_ -> IO Bool
pairDevice_address_port_setupPINCode_error mtrDeviceController  deviceID address port setupPINCode error_ =
  withObjCPtr address $ \raw_address ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "pairDevice:address:port:setupPINCode:error:") retCULong [argCULong deviceID, argPtr (castPtr raw_address :: Ptr ()), argCUInt (fromIntegral port), argCUInt setupPINCode, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- pairDevice:onboardingPayload:error:@
pairDevice_onboardingPayload_error :: (IsMTRDeviceController mtrDeviceController, IsNSString onboardingPayload, IsNSError error_) => mtrDeviceController -> CULong -> onboardingPayload -> error_ -> IO Bool
pairDevice_onboardingPayload_error mtrDeviceController  deviceID onboardingPayload error_ =
  withObjCPtr onboardingPayload $ \raw_onboardingPayload ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "pairDevice:onboardingPayload:error:") retCULong [argCULong deviceID, argPtr (castPtr raw_onboardingPayload :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- commissionDevice:commissioningParams:error:@
commissionDevice_commissioningParams_error :: (IsMTRDeviceController mtrDeviceController, IsMTRCommissioningParameters commissioningParams, IsNSError error_) => mtrDeviceController -> CULong -> commissioningParams -> error_ -> IO Bool
commissionDevice_commissioningParams_error mtrDeviceController  deviceId commissioningParams error_ =
  withObjCPtr commissioningParams $ \raw_commissioningParams ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "commissionDevice:commissioningParams:error:") retCULong [argCULong deviceId, argPtr (castPtr raw_commissioningParams :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- stopDevicePairing:error:@
stopDevicePairing_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> CULong -> error_ -> IO Bool
stopDevicePairing_error mtrDeviceController  deviceID error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "stopDevicePairing:error:") retCULong [argCULong deviceID, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- getDeviceBeingCommissioned:error:@
getDeviceBeingCommissioned_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> CULong -> error_ -> IO (Id MTRBaseDevice)
getDeviceBeingCommissioned_error mtrDeviceController  deviceId error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg mtrDeviceController (mkSelector "getDeviceBeingCommissioned:error:") (retPtr retVoid) [argCULong deviceId, argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- openPairingWindow:duration:error:@
openPairingWindow_duration_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> CULong -> CULong -> error_ -> IO Bool
openPairingWindow_duration_error mtrDeviceController  deviceID duration error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "openPairingWindow:duration:error:") retCULong [argCULong deviceID, argCULong duration, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- openPairingWindowWithPIN:duration:discriminator:setupPIN:error:@
openPairingWindowWithPIN_duration_discriminator_setupPIN_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> CULong -> CULong -> CULong -> CULong -> error_ -> IO (Id NSString)
openPairingWindowWithPIN_duration_discriminator_setupPIN_error mtrDeviceController  deviceID duration discriminator setupPIN error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg mtrDeviceController (mkSelector "openPairingWindowWithPIN:duration:discriminator:setupPIN:error:") (retPtr retVoid) [argCULong deviceID, argCULong duration, argCULong discriminator, argCULong setupPIN, argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- computePaseVerifier:iterations:salt:@
computePaseVerifier_iterations_salt :: (IsMTRDeviceController mtrDeviceController, IsNSData salt) => mtrDeviceController -> CUInt -> CUInt -> salt -> IO (Id NSData)
computePaseVerifier_iterations_salt mtrDeviceController  setupPincode iterations salt =
  withObjCPtr salt $ \raw_salt ->
      sendMsg mtrDeviceController (mkSelector "computePaseVerifier:iterations:salt:") (retPtr retVoid) [argCUInt setupPincode, argCUInt iterations, argPtr (castPtr raw_salt :: Ptr ())] >>= retainedObject . castPtr

-- | @- setPairingDelegate:queue:@
setPairingDelegate_queue :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> RawId -> queue -> IO ()
setPairingDelegate_queue mtrDeviceController  delegate queue =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrDeviceController (mkSelector "setPairingDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | @- setNocChainIssuer:queue:@
setNocChainIssuer_queue :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> RawId -> queue -> IO ()
setNocChainIssuer_queue mtrDeviceController  nocChainIssuer queue =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrDeviceController (mkSelector "setNocChainIssuer:queue:") retVoid [argPtr (castPtr (unRawId nocChainIssuer) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | If true, the controller has not been shut down yet.
--
-- ObjC selector: @- running@
running :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO Bool
running mtrDeviceController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "running") retCULong []

-- | If true, the controller has been suspended via @suspend@ and not resumed yet.
--
-- ObjC selector: @- suspended@
suspended :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO Bool
suspended mtrDeviceController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceController (mkSelector "suspended") retCULong []

-- | The ID assigned to this controller at creation time.
--
-- ObjC selector: @- uniqueIdentifier@
uniqueIdentifier :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id NSUUID)
uniqueIdentifier mtrDeviceController  =
    sendMsg mtrDeviceController (mkSelector "uniqueIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Return the Node ID assigned to the controller.  Will return nil if the controller is not running (and hence does not know its node id).
--
-- ObjC selector: @- controllerNodeID@
controllerNodeID :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id NSNumber)
controllerNodeID mtrDeviceController  =
    sendMsg mtrDeviceController (mkSelector "controllerNodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the list of MTRDevice instances that this controller has loaded into memory. Returns an empty array if no devices are in memory.
--
-- ObjC selector: @- devices@
devices :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id NSArray)
devices mtrDeviceController  =
    sendMsg mtrDeviceController (mkSelector "devices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the list of node IDs for which this controller has stored information.  Returns empty list if the controller does not have any information stored.
--
-- ObjC selector: @- nodesWithStoredData@
nodesWithStoredData :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id NSArray)
nodesWithStoredData mtrDeviceController  =
    sendMsg mtrDeviceController (mkSelector "nodesWithStoredData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- controllerNodeId@
controllerNodeId :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id NSNumber)
controllerNodeId mtrDeviceController  =
    sendMsg mtrDeviceController (mkSelector "controllerNodeId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithParameters:error:@
initWithParameters_errorSelector :: Selector
initWithParameters_errorSelector = mkSelector "initWithParameters:error:"

-- | @Selector@ for @setupCommissioningSessionWithPayload:newNodeID:error:@
setupCommissioningSessionWithPayload_newNodeID_errorSelector :: Selector
setupCommissioningSessionWithPayload_newNodeID_errorSelector = mkSelector "setupCommissioningSessionWithPayload:newNodeID:error:"

-- | @Selector@ for @setupCommissioningSessionWithDiscoveredDevice:payload:newNodeID:error:@
setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_errorSelector :: Selector
setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_errorSelector = mkSelector "setupCommissioningSessionWithDiscoveredDevice:payload:newNodeID:error:"

-- | @Selector@ for @commissionNodeWithID:commissioningParams:error:@
commissionNodeWithID_commissioningParams_errorSelector :: Selector
commissionNodeWithID_commissioningParams_errorSelector = mkSelector "commissionNodeWithID:commissioningParams:error:"

-- | @Selector@ for @continueCommissioningDevice:ignoreAttestationFailure:error:@
continueCommissioningDevice_ignoreAttestationFailure_errorSelector :: Selector
continueCommissioningDevice_ignoreAttestationFailure_errorSelector = mkSelector "continueCommissioningDevice:ignoreAttestationFailure:error:"

-- | @Selector@ for @cancelCommissioningForNodeID:error:@
cancelCommissioningForNodeID_errorSelector :: Selector
cancelCommissioningForNodeID_errorSelector = mkSelector "cancelCommissioningForNodeID:error:"

-- | @Selector@ for @deviceBeingCommissionedWithNodeID:error:@
deviceBeingCommissionedWithNodeID_errorSelector :: Selector
deviceBeingCommissionedWithNodeID_errorSelector = mkSelector "deviceBeingCommissionedWithNodeID:error:"

-- | @Selector@ for @preWarmCommissioningSession@
preWarmCommissioningSessionSelector :: Selector
preWarmCommissioningSessionSelector = mkSelector "preWarmCommissioningSession"

-- | @Selector@ for @setDeviceControllerDelegate:queue:@
setDeviceControllerDelegate_queueSelector :: Selector
setDeviceControllerDelegate_queueSelector = mkSelector "setDeviceControllerDelegate:queue:"

-- | @Selector@ for @addDeviceControllerDelegate:queue:@
addDeviceControllerDelegate_queueSelector :: Selector
addDeviceControllerDelegate_queueSelector = mkSelector "addDeviceControllerDelegate:queue:"

-- | @Selector@ for @removeDeviceControllerDelegate:@
removeDeviceControllerDelegateSelector :: Selector
removeDeviceControllerDelegateSelector = mkSelector "removeDeviceControllerDelegate:"

-- | @Selector@ for @startBrowseForCommissionables:queue:@
startBrowseForCommissionables_queueSelector :: Selector
startBrowseForCommissionables_queueSelector = mkSelector "startBrowseForCommissionables:queue:"

-- | @Selector@ for @stopBrowseForCommissionables@
stopBrowseForCommissionablesSelector :: Selector
stopBrowseForCommissionablesSelector = mkSelector "stopBrowseForCommissionables"

-- | @Selector@ for @attestationChallengeForDeviceID:@
attestationChallengeForDeviceIDSelector :: Selector
attestationChallengeForDeviceIDSelector = mkSelector "attestationChallengeForDeviceID:"

-- | @Selector@ for @addServerEndpoint:@
addServerEndpointSelector :: Selector
addServerEndpointSelector = mkSelector "addServerEndpoint:"

-- | @Selector@ for @removeServerEndpoint:queue:completion:@
removeServerEndpoint_queue_completionSelector :: Selector
removeServerEndpoint_queue_completionSelector = mkSelector "removeServerEndpoint:queue:completion:"

-- | @Selector@ for @removeServerEndpoint:@
removeServerEndpointSelector :: Selector
removeServerEndpointSelector = mkSelector "removeServerEndpoint:"

-- | @Selector@ for @forgetDeviceWithNodeID:@
forgetDeviceWithNodeIDSelector :: Selector
forgetDeviceWithNodeIDSelector = mkSelector "forgetDeviceWithNodeID:"

-- | @Selector@ for @computePASEVerifierForSetupPasscode:iterations:salt:error:@
computePASEVerifierForSetupPasscode_iterations_salt_errorSelector :: Selector
computePASEVerifierForSetupPasscode_iterations_salt_errorSelector = mkSelector "computePASEVerifierForSetupPasscode:iterations:salt:error:"

-- | @Selector@ for @suspend@
suspendSelector :: Selector
suspendSelector = mkSelector "suspend"

-- | @Selector@ for @resume@
resumeSelector :: Selector
resumeSelector = mkSelector "resume"

-- | @Selector@ for @shutdown@
shutdownSelector :: Selector
shutdownSelector = mkSelector "shutdown"

-- | @Selector@ for @sharedControllerWithId:xpcConnectBlock:@
sharedControllerWithId_xpcConnectBlockSelector :: Selector
sharedControllerWithId_xpcConnectBlockSelector = mkSelector "sharedControllerWithId:xpcConnectBlock:"

-- | @Selector@ for @sharedControllerWithID:xpcConnectBlock:@
sharedControllerWithID_xpcConnectBlockSelector :: Selector
sharedControllerWithID_xpcConnectBlockSelector = mkSelector "sharedControllerWithID:xpcConnectBlock:"

-- | @Selector@ for @encodeXPCResponseValues:@
encodeXPCResponseValuesSelector :: Selector
encodeXPCResponseValuesSelector = mkSelector "encodeXPCResponseValues:"

-- | @Selector@ for @decodeXPCResponseValues:@
decodeXPCResponseValuesSelector :: Selector
decodeXPCResponseValuesSelector = mkSelector "decodeXPCResponseValues:"

-- | @Selector@ for @encodeXPCReadParams:@
encodeXPCReadParamsSelector :: Selector
encodeXPCReadParamsSelector = mkSelector "encodeXPCReadParams:"

-- | @Selector@ for @decodeXPCReadParams:@
decodeXPCReadParamsSelector :: Selector
decodeXPCReadParamsSelector = mkSelector "decodeXPCReadParams:"

-- | @Selector@ for @encodeXPCSubscribeParams:@
encodeXPCSubscribeParamsSelector :: Selector
encodeXPCSubscribeParamsSelector = mkSelector "encodeXPCSubscribeParams:"

-- | @Selector@ for @decodeXPCSubscribeParams:@
decodeXPCSubscribeParamsSelector :: Selector
decodeXPCSubscribeParamsSelector = mkSelector "decodeXPCSubscribeParams:"

-- | @Selector@ for @xpcInterfaceForServerProtocol@
xpcInterfaceForServerProtocolSelector :: Selector
xpcInterfaceForServerProtocolSelector = mkSelector "xpcInterfaceForServerProtocol"

-- | @Selector@ for @xpcInterfaceForClientProtocol@
xpcInterfaceForClientProtocolSelector :: Selector
xpcInterfaceForClientProtocolSelector = mkSelector "xpcInterfaceForClientProtocol"

-- | @Selector@ for @fetchAttestationChallengeForDeviceId:@
fetchAttestationChallengeForDeviceIdSelector :: Selector
fetchAttestationChallengeForDeviceIdSelector = mkSelector "fetchAttestationChallengeForDeviceId:"

-- | @Selector@ for @getBaseDevice:queue:completionHandler:@
getBaseDevice_queue_completionHandlerSelector :: Selector
getBaseDevice_queue_completionHandlerSelector = mkSelector "getBaseDevice:queue:completionHandler:"

-- | @Selector@ for @pairDevice:discriminator:setupPINCode:error:@
pairDevice_discriminator_setupPINCode_errorSelector :: Selector
pairDevice_discriminator_setupPINCode_errorSelector = mkSelector "pairDevice:discriminator:setupPINCode:error:"

-- | @Selector@ for @pairDevice:address:port:setupPINCode:error:@
pairDevice_address_port_setupPINCode_errorSelector :: Selector
pairDevice_address_port_setupPINCode_errorSelector = mkSelector "pairDevice:address:port:setupPINCode:error:"

-- | @Selector@ for @pairDevice:onboardingPayload:error:@
pairDevice_onboardingPayload_errorSelector :: Selector
pairDevice_onboardingPayload_errorSelector = mkSelector "pairDevice:onboardingPayload:error:"

-- | @Selector@ for @commissionDevice:commissioningParams:error:@
commissionDevice_commissioningParams_errorSelector :: Selector
commissionDevice_commissioningParams_errorSelector = mkSelector "commissionDevice:commissioningParams:error:"

-- | @Selector@ for @stopDevicePairing:error:@
stopDevicePairing_errorSelector :: Selector
stopDevicePairing_errorSelector = mkSelector "stopDevicePairing:error:"

-- | @Selector@ for @getDeviceBeingCommissioned:error:@
getDeviceBeingCommissioned_errorSelector :: Selector
getDeviceBeingCommissioned_errorSelector = mkSelector "getDeviceBeingCommissioned:error:"

-- | @Selector@ for @openPairingWindow:duration:error:@
openPairingWindow_duration_errorSelector :: Selector
openPairingWindow_duration_errorSelector = mkSelector "openPairingWindow:duration:error:"

-- | @Selector@ for @openPairingWindowWithPIN:duration:discriminator:setupPIN:error:@
openPairingWindowWithPIN_duration_discriminator_setupPIN_errorSelector :: Selector
openPairingWindowWithPIN_duration_discriminator_setupPIN_errorSelector = mkSelector "openPairingWindowWithPIN:duration:discriminator:setupPIN:error:"

-- | @Selector@ for @computePaseVerifier:iterations:salt:@
computePaseVerifier_iterations_saltSelector :: Selector
computePaseVerifier_iterations_saltSelector = mkSelector "computePaseVerifier:iterations:salt:"

-- | @Selector@ for @setPairingDelegate:queue:@
setPairingDelegate_queueSelector :: Selector
setPairingDelegate_queueSelector = mkSelector "setPairingDelegate:queue:"

-- | @Selector@ for @setNocChainIssuer:queue:@
setNocChainIssuer_queueSelector :: Selector
setNocChainIssuer_queueSelector = mkSelector "setNocChainIssuer:queue:"

-- | @Selector@ for @running@
runningSelector :: Selector
runningSelector = mkSelector "running"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @controllerNodeID@
controllerNodeIDSelector :: Selector
controllerNodeIDSelector = mkSelector "controllerNodeID"

-- | @Selector@ for @devices@
devicesSelector :: Selector
devicesSelector = mkSelector "devices"

-- | @Selector@ for @nodesWithStoredData@
nodesWithStoredDataSelector :: Selector
nodesWithStoredDataSelector = mkSelector "nodesWithStoredData"

-- | @Selector@ for @controllerNodeId@
controllerNodeIdSelector :: Selector
controllerNodeIdSelector = mkSelector "controllerNodeId"

