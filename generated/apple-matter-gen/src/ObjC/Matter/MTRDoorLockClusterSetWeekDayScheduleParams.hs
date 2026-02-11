{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetWeekDayScheduleParams@.
module ObjC.Matter.MTRDoorLockClusterSetWeekDayScheduleParams
  ( MTRDoorLockClusterSetWeekDayScheduleParams
  , IsMTRDoorLockClusterSetWeekDayScheduleParams(..)
  , weekDayIndex
  , setWeekDayIndex
  , userIndex
  , setUserIndex
  , daysMask
  , setDaysMask
  , startHour
  , setStartHour
  , startMinute
  , setStartMinute
  , endHour
  , setEndHour
  , endMinute
  , setEndMinute
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , weekDayIndexSelector
  , setWeekDayIndexSelector
  , userIndexSelector
  , setUserIndexSelector
  , daysMaskSelector
  , setDaysMaskSelector
  , startHourSelector
  , setStartHourSelector
  , startMinuteSelector
  , setStartMinuteSelector
  , endHourSelector
  , setEndHourSelector
  , endMinuteSelector
  , setEndMinuteSelector
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
weekDayIndex :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
weekDayIndex mtrDoorLockClusterSetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "weekDayIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWeekDayIndex:@
setWeekDayIndex :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setWeekDayIndex mtrDoorLockClusterSetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "setWeekDayIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterSetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setUserIndex mtrDoorLockClusterSetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- daysMask@
daysMask :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
daysMask mtrDoorLockClusterSetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "daysMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDaysMask:@
setDaysMask :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setDaysMask mtrDoorLockClusterSetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "setDaysMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startHour@
startHour :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
startHour mtrDoorLockClusterSetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "startHour") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartHour:@
setStartHour :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setStartHour mtrDoorLockClusterSetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "setStartHour:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startMinute@
startMinute :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
startMinute mtrDoorLockClusterSetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "startMinute") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartMinute:@
setStartMinute :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setStartMinute mtrDoorLockClusterSetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "setStartMinute:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endHour@
endHour :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
endHour mtrDoorLockClusterSetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "endHour") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndHour:@
setEndHour :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setEndHour mtrDoorLockClusterSetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "setEndHour:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endMinute@
endMinute :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
endMinute mtrDoorLockClusterSetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "endMinute") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndMinute:@
setEndMinute :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setEndMinute mtrDoorLockClusterSetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "setEndMinute:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterSetWeekDayScheduleParams  =
    sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterSetWeekDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetWeekDayScheduleParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @daysMask@
daysMaskSelector :: Selector
daysMaskSelector = mkSelector "daysMask"

-- | @Selector@ for @setDaysMask:@
setDaysMaskSelector :: Selector
setDaysMaskSelector = mkSelector "setDaysMask:"

-- | @Selector@ for @startHour@
startHourSelector :: Selector
startHourSelector = mkSelector "startHour"

-- | @Selector@ for @setStartHour:@
setStartHourSelector :: Selector
setStartHourSelector = mkSelector "setStartHour:"

-- | @Selector@ for @startMinute@
startMinuteSelector :: Selector
startMinuteSelector = mkSelector "startMinute"

-- | @Selector@ for @setStartMinute:@
setStartMinuteSelector :: Selector
setStartMinuteSelector = mkSelector "setStartMinute:"

-- | @Selector@ for @endHour@
endHourSelector :: Selector
endHourSelector = mkSelector "endHour"

-- | @Selector@ for @setEndHour:@
setEndHourSelector :: Selector
setEndHourSelector = mkSelector "setEndHour:"

-- | @Selector@ for @endMinute@
endMinuteSelector :: Selector
endMinuteSelector = mkSelector "endMinute"

-- | @Selector@ for @setEndMinute:@
setEndMinuteSelector :: Selector
setEndMinuteSelector = mkSelector "setEndMinute:"

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

