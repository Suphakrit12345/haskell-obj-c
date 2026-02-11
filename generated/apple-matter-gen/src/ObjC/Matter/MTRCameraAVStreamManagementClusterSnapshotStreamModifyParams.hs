{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSnapshotStreamModifyParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSnapshotStreamModifyParams
  ( MTRCameraAVStreamManagementClusterSnapshotStreamModifyParams
  , IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams(..)
  , snapshotStreamID
  , setSnapshotStreamID
  , watermarkEnabled
  , setWatermarkEnabled
  , osdEnabled
  , setOsdEnabled
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , snapshotStreamIDSelector
  , setSnapshotStreamIDSelector
  , watermarkEnabledSelector
  , setWatermarkEnabledSelector
  , osdEnabledSelector
  , setOsdEnabledSelector
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
snapshotStreamID :: IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> IO (Id NSNumber)
snapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams (mkSelector "snapshotStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSnapshotStreamID:@
setSnapshotStreamID :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> value -> IO ()
setSnapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams (mkSelector "setSnapshotStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- watermarkEnabled@
watermarkEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> IO (Id NSNumber)
watermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams (mkSelector "watermarkEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWatermarkEnabled:@
setWatermarkEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> value -> IO ()
setWatermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams (mkSelector "setWatermarkEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- osdEnabled@
osdEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> IO (Id NSNumber)
osdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams (mkSelector "osdEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOsdEnabled:@
setOsdEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> value -> IO ()
setOsdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams (mkSelector "setOsdEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @snapshotStreamID@
snapshotStreamIDSelector :: Selector
snapshotStreamIDSelector = mkSelector "snapshotStreamID"

-- | @Selector@ for @setSnapshotStreamID:@
setSnapshotStreamIDSelector :: Selector
setSnapshotStreamIDSelector = mkSelector "setSnapshotStreamID:"

-- | @Selector@ for @watermarkEnabled@
watermarkEnabledSelector :: Selector
watermarkEnabledSelector = mkSelector "watermarkEnabled"

-- | @Selector@ for @setWatermarkEnabled:@
setWatermarkEnabledSelector :: Selector
setWatermarkEnabledSelector = mkSelector "setWatermarkEnabled:"

-- | @Selector@ for @osdEnabled@
osdEnabledSelector :: Selector
osdEnabledSelector = mkSelector "osdEnabled"

-- | @Selector@ for @setOsdEnabled:@
setOsdEnabledSelector :: Selector
setOsdEnabledSelector = mkSelector "setOsdEnabled:"

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

