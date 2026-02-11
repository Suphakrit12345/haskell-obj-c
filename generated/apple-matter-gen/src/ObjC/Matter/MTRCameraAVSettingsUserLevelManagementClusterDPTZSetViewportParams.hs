{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams
  ( MTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams
  , IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams(..)
  , videoStreamID
  , setVideoStreamID
  , viewport
  , setViewport
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , videoStreamIDSelector
  , setVideoStreamIDSelector
  , viewportSelector
  , setViewportSelector
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
videoStreamID :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> IO (Id NSNumber)
videoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> value -> IO ()
setVideoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- viewport@
viewport :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> IO (Id MTRDataTypeViewportStruct)
viewport mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams (mkSelector "viewport") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setViewport:@
setViewport :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams, IsMTRDataTypeViewportStruct value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> value -> IO ()
setViewport mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams (mkSelector "setViewport:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @viewport@
viewportSelector :: Selector
viewportSelector = mkSelector "viewport"

-- | @Selector@ for @setViewport:@
setViewportSelector :: Selector
setViewportSelector = mkSelector "setViewport:"

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

