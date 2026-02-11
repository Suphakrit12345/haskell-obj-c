{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams
  ( MTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams
  , IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams(..)
  , pan
  , setPan
  , tilt
  , setTilt
  , zoom
  , setZoom
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , panSelector
  , setPanSelector
  , tiltSelector
  , setTiltSelector
  , zoomSelector
  , setZoomSelector
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

-- | @- pan@
pan :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> IO (Id NSNumber)
pan mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams (mkSelector "pan") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPan:@
setPan :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> value -> IO ()
setPan mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams (mkSelector "setPan:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tilt@
tilt :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> IO (Id NSNumber)
tilt mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams (mkSelector "tilt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTilt:@
setTilt :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> value -> IO ()
setTilt mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams (mkSelector "setTilt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- zoom@
zoom :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> IO (Id NSNumber)
zoom mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams (mkSelector "zoom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZoom:@
setZoom :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> value -> IO ()
setZoom mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams (mkSelector "setZoom:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pan@
panSelector :: Selector
panSelector = mkSelector "pan"

-- | @Selector@ for @setPan:@
setPanSelector :: Selector
setPanSelector = mkSelector "setPan:"

-- | @Selector@ for @tilt@
tiltSelector :: Selector
tiltSelector = mkSelector "tilt"

-- | @Selector@ for @setTilt:@
setTiltSelector :: Selector
setTiltSelector = mkSelector "setTilt:"

-- | @Selector@ for @zoom@
zoomSelector :: Selector
zoomSelector = mkSelector "zoom"

-- | @Selector@ for @setZoom:@
setZoomSelector :: Selector
setZoomSelector = mkSelector "setZoom:"

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

