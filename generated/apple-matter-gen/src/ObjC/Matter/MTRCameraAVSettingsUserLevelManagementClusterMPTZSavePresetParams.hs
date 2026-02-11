{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams
  ( MTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams
  , IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams(..)
  , presetID
  , setPresetID
  , name
  , setName
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , presetIDSelector
  , setPresetIDSelector
  , nameSelector
  , setNameSelector
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
presetID :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> IO (Id NSNumber)
presetID mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams (mkSelector "presetID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPresetID:@
setPresetID :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> value -> IO ()
setPresetID mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams (mkSelector "setPresetID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> IO (Id NSString)
name mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams, IsNSString value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> value -> IO ()
setName mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presetID@
presetIDSelector :: Selector
presetIDSelector = mkSelector "presetID"

-- | @Selector@ for @setPresetID:@
setPresetIDSelector :: Selector
setPresetIDSelector = mkSelector "setPresetID:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

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

