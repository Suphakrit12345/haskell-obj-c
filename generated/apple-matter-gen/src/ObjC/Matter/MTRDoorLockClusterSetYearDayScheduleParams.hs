{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetYearDayScheduleParams@.
module ObjC.Matter.MTRDoorLockClusterSetYearDayScheduleParams
  ( MTRDoorLockClusterSetYearDayScheduleParams
  , IsMTRDoorLockClusterSetYearDayScheduleParams(..)
  , yearDayIndex
  , setYearDayIndex
  , userIndex
  , setUserIndex
  , localStartTime
  , setLocalStartTime
  , localEndTime
  , setLocalEndTime
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , yearDayIndexSelector
  , setYearDayIndexSelector
  , userIndexSelector
  , setUserIndexSelector
  , localStartTimeSelector
  , setLocalStartTimeSelector
  , localEndTimeSelector
  , setLocalEndTimeSelector
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

-- | @- yearDayIndex@
yearDayIndex :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
yearDayIndex mtrDoorLockClusterSetYearDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "yearDayIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setYearDayIndex:@
setYearDayIndex :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setYearDayIndex mtrDoorLockClusterSetYearDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "setYearDayIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterSetYearDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setUserIndex mtrDoorLockClusterSetYearDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localStartTime@
localStartTime :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
localStartTime mtrDoorLockClusterSetYearDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "localStartTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalStartTime:@
setLocalStartTime :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setLocalStartTime mtrDoorLockClusterSetYearDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "setLocalStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localEndTime@
localEndTime :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
localEndTime mtrDoorLockClusterSetYearDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "localEndTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalEndTime:@
setLocalEndTime :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setLocalEndTime mtrDoorLockClusterSetYearDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "setLocalEndTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetYearDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetYearDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterSetYearDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterSetYearDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetYearDayScheduleParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @yearDayIndex@
yearDayIndexSelector :: Selector
yearDayIndexSelector = mkSelector "yearDayIndex"

-- | @Selector@ for @setYearDayIndex:@
setYearDayIndexSelector :: Selector
setYearDayIndexSelector = mkSelector "setYearDayIndex:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @localStartTime@
localStartTimeSelector :: Selector
localStartTimeSelector = mkSelector "localStartTime"

-- | @Selector@ for @setLocalStartTime:@
setLocalStartTimeSelector :: Selector
setLocalStartTimeSelector = mkSelector "setLocalStartTime:"

-- | @Selector@ for @localEndTime@
localEndTimeSelector :: Selector
localEndTimeSelector = mkSelector "localEndTime"

-- | @Selector@ for @setLocalEndTime:@
setLocalEndTimeSelector :: Selector
setLocalEndTimeSelector = mkSelector "setLocalEndTime:"

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

