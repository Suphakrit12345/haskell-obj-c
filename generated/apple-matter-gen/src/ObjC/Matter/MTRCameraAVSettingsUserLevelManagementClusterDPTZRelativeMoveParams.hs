{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams
  ( MTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams
  , IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams(..)
  , videoStreamID
  , setVideoStreamID
  , deltaX
  , setDeltaX
  , deltaY
  , setDeltaY
  , zoomDelta
  , setZoomDelta
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , videoStreamIDSelector
  , setVideoStreamIDSelector
  , deltaXSelector
  , setDeltaXSelector
  , deltaYSelector
  , setDeltaYSelector
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

-- | @- videoStreamID@
videoStreamID :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
videoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setVideoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deltaX@
deltaX :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
deltaX mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "deltaX") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeltaX:@
setDeltaX :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setDeltaX mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "setDeltaX:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deltaY@
deltaY :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
deltaY mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "deltaY") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeltaY:@
setDeltaY :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setDeltaY mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "setDeltaY:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- zoomDelta@
zoomDelta :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
zoomDelta mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "zoomDelta") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZoomDelta:@
setZoomDelta :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setZoomDelta mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "setZoomDelta:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @deltaX@
deltaXSelector :: Selector
deltaXSelector = mkSelector "deltaX"

-- | @Selector@ for @setDeltaX:@
setDeltaXSelector :: Selector
setDeltaXSelector = mkSelector "setDeltaX:"

-- | @Selector@ for @deltaY@
deltaYSelector :: Selector
deltaYSelector = mkSelector "deltaY"

-- | @Selector@ for @setDeltaY:@
setDeltaYSelector :: Selector
setDeltaYSelector = mkSelector "setDeltaY:"

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

