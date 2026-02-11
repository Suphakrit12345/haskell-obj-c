{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams
  ( MTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams
  , IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams(..)
  , panDelta
  , setPanDelta
  , tiltDelta
  , setTiltDelta
  , zoomDelta
  , setZoomDelta
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , panDeltaSelector
  , setPanDeltaSelector
  , tiltDeltaSelector
  , setTiltDeltaSelector
  , zoomDeltaSelector
  , setZoomDeltaSelector
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

-- | @- panDelta@
panDelta :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams -> IO (Id NSNumber)
panDelta mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams (mkSelector "panDelta") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPanDelta:@
setPanDelta :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams -> value -> IO ()
setPanDelta mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams (mkSelector "setPanDelta:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tiltDelta@
tiltDelta :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams -> IO (Id NSNumber)
tiltDelta mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams (mkSelector "tiltDelta") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTiltDelta:@
setTiltDelta :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams -> value -> IO ()
setTiltDelta mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams (mkSelector "setTiltDelta:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- zoomDelta@
zoomDelta :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams -> IO (Id NSNumber)
zoomDelta mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams (mkSelector "zoomDelta") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZoomDelta:@
setZoomDelta :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams -> value -> IO ()
setZoomDelta mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams (mkSelector "setZoomDelta:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @panDelta@
panDeltaSelector :: Selector
panDeltaSelector = mkSelector "panDelta"

-- | @Selector@ for @setPanDelta:@
setPanDeltaSelector :: Selector
setPanDeltaSelector = mkSelector "setPanDelta:"

-- | @Selector@ for @tiltDelta@
tiltDeltaSelector :: Selector
tiltDeltaSelector = mkSelector "tiltDelta"

-- | @Selector@ for @setTiltDelta:@
setTiltDeltaSelector :: Selector
setTiltDeltaSelector = mkSelector "setTiltDelta:"

-- | @Selector@ for @zoomDelta@
zoomDeltaSelector :: Selector
zoomDeltaSelector = mkSelector "zoomDelta"

-- | @Selector@ for @setZoomDelta:@
setZoomDeltaSelector :: Selector
setZoomDeltaSelector = mkSelector "setZoomDelta:"

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

