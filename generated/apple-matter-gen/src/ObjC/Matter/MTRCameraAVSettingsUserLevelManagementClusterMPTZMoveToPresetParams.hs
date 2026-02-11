{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams
  ( MTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams
  , IsMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams(..)
  , presetID
  , setPresetID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , presetIDSelector
  , setPresetIDSelector
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

-- | @- presetID@
presetID :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams -> IO (Id NSNumber)
presetID mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams (mkSelector "presetID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPresetID:@
setPresetID :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams -> value -> IO ()
setPresetID mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams (mkSelector "setPresetID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presetID@
presetIDSelector :: Selector
presetIDSelector = mkSelector "presetID"

-- | @Selector@ for @setPresetID:@
setPresetIDSelector :: Selector
setPresetIDSelector = mkSelector "setPresetID:"

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

