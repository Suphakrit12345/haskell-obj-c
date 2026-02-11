{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoStreamDeallocateParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoStreamDeallocateParams
  ( MTRCameraAVStreamManagementClusterVideoStreamDeallocateParams
  , IsMTRCameraAVStreamManagementClusterVideoStreamDeallocateParams(..)
  , videoStreamID
  , setVideoStreamID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , videoStreamIDSelector
  , setVideoStreamIDSelector
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
videoStreamID :: IsMTRCameraAVStreamManagementClusterVideoStreamDeallocateParams mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams => mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams -> IO (Id NSNumber)
videoStreamID mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVStreamManagementClusterVideoStreamDeallocateParams mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams -> value -> IO ()
setVideoStreamID mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterVideoStreamDeallocateParams mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams => mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterVideoStreamDeallocateParams mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterVideoStreamDeallocateParams mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams => mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterVideoStreamDeallocateParams mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamDeallocateParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

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

