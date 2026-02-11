{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterGetWeekDayScheduleParams@.
module ObjC.Matter.MTRDoorLockClusterGetWeekDayScheduleParams
  ( MTRDoorLockClusterGetWeekDayScheduleParams
  , IsMTRDoorLockClusterGetWeekDayScheduleParams(..)
  , weekDayIndex
  , setWeekDayIndex
  , userIndex
  , setUserIndex
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , weekDayIndexSelector
  , setWeekDayIndexSelector
  , userIndexSelector
  , setUserIndexSelector
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

-- | @- weekDayIndex@
weekDayIndex :: IsMTRDoorLockClusterGetWeekDayScheduleParams mtrDoorLockClusterGetWeekDayScheduleParams => mtrDoorLockClusterGetWeekDayScheduleParams -> IO (Id NSNumber)
weekDayIndex mtrDoorLockClusterGetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleParams (mkSelector "weekDayIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWeekDayIndex:@
setWeekDayIndex :: (IsMTRDoorLockClusterGetWeekDayScheduleParams mtrDoorLockClusterGetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleParams -> value -> IO ()
setWeekDayIndex mtrDoorLockClusterGetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleParams (mkSelector "setWeekDayIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterGetWeekDayScheduleParams mtrDoorLockClusterGetWeekDayScheduleParams => mtrDoorLockClusterGetWeekDayScheduleParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterGetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterGetWeekDayScheduleParams mtrDoorLockClusterGetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleParams -> value -> IO ()
setUserIndex mtrDoorLockClusterGetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterGetWeekDayScheduleParams mtrDoorLockClusterGetWeekDayScheduleParams => mtrDoorLockClusterGetWeekDayScheduleParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterGetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterGetWeekDayScheduleParams mtrDoorLockClusterGetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterGetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterGetWeekDayScheduleParams mtrDoorLockClusterGetWeekDayScheduleParams => mtrDoorLockClusterGetWeekDayScheduleParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterGetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterGetWeekDayScheduleParams mtrDoorLockClusterGetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterGetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @weekDayIndex@
weekDayIndexSelector :: Selector
weekDayIndexSelector = mkSelector "weekDayIndex"

-- | @Selector@ for @setWeekDayIndex:@
setWeekDayIndexSelector :: Selector
setWeekDayIndexSelector = mkSelector "setWeekDayIndex:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector
setUserIndexSelector = mkSelector "setUserIndex:"

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

