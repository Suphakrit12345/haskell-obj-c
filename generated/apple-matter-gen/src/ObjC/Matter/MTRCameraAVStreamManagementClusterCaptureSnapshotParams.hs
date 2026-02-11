{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterCaptureSnapshotParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterCaptureSnapshotParams
  ( MTRCameraAVStreamManagementClusterCaptureSnapshotParams
  , IsMTRCameraAVStreamManagementClusterCaptureSnapshotParams(..)
  , snapshotStreamID
  , setSnapshotStreamID
  , requestedResolution
  , setRequestedResolution
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , snapshotStreamIDSelector
  , setSnapshotStreamIDSelector
  , requestedResolutionSelector
  , setRequestedResolutionSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector


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

-- | @- snapshotStreamID@
snapshotStreamID :: IsMTRCameraAVStreamManagementClusterCaptureSnapshotParams mtrCameraAVStreamManagementClusterCaptureSnapshotParams => mtrCameraAVStreamManagementClusterCaptureSnapshotParams -> IO (Id NSNumber)
snapshotStreamID mtrCameraAVStreamManagementClusterCaptureSnapshotParams  =
    sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotParams (mkSelector "snapshotStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSnapshotStreamID:@
setSnapshotStreamID :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotParams mtrCameraAVStreamManagementClusterCaptureSnapshotParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterCaptureSnapshotParams -> value -> IO ()
setSnapshotStreamID mtrCameraAVStreamManagementClusterCaptureSnapshotParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotParams (mkSelector "setSnapshotStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requestedResolution@
requestedResolution :: IsMTRCameraAVStreamManagementClusterCaptureSnapshotParams mtrCameraAVStreamManagementClusterCaptureSnapshotParams => mtrCameraAVStreamManagementClusterCaptureSnapshotParams -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
requestedResolution mtrCameraAVStreamManagementClusterCaptureSnapshotParams  =
    sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotParams (mkSelector "requestedResolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequestedResolution:@
setRequestedResolution :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotParams mtrCameraAVStreamManagementClusterCaptureSnapshotParams, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterCaptureSnapshotParams -> value -> IO ()
setRequestedResolution mtrCameraAVStreamManagementClusterCaptureSnapshotParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotParams (mkSelector "setRequestedResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterCaptureSnapshotParams mtrCameraAVStreamManagementClusterCaptureSnapshotParams => mtrCameraAVStreamManagementClusterCaptureSnapshotParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterCaptureSnapshotParams  =
    sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotParams mtrCameraAVStreamManagementClusterCaptureSnapshotParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterCaptureSnapshotParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterCaptureSnapshotParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterCaptureSnapshotParams mtrCameraAVStreamManagementClusterCaptureSnapshotParams => mtrCameraAVStreamManagementClusterCaptureSnapshotParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterCaptureSnapshotParams  =
    sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotParams mtrCameraAVStreamManagementClusterCaptureSnapshotParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterCaptureSnapshotParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterCaptureSnapshotParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @snapshotStreamID@
snapshotStreamIDSelector :: Selector
snapshotStreamIDSelector = mkSelector "snapshotStreamID"

-- | @Selector@ for @setSnapshotStreamID:@
setSnapshotStreamIDSelector :: Selector
setSnapshotStreamIDSelector = mkSelector "setSnapshotStreamID:"

-- | @Selector@ for @requestedResolution@
requestedResolutionSelector :: Selector
requestedResolutionSelector = mkSelector "requestedResolution"

-- | @Selector@ for @setRequestedResolution:@
setRequestedResolutionSelector :: Selector
setRequestedResolutionSelector = mkSelector "setRequestedResolution:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

