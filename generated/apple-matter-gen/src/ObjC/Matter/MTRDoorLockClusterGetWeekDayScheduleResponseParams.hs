{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterGetWeekDayScheduleResponseParams@.
module ObjC.Matter.MTRDoorLockClusterGetWeekDayScheduleResponseParams
  ( MTRDoorLockClusterGetWeekDayScheduleResponseParams
  , IsMTRDoorLockClusterGetWeekDayScheduleResponseParams(..)
  , initWithResponseValue_error
  , weekDayIndex
  , setWeekDayIndex
  , userIndex
  , setUserIndex
  , status
  , setStatus
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
  , initWithResponseValue_errorSelector
  , weekDayIndexSelector
  , setWeekDayIndexSelector
  , userIndexSelector
  , setUserIndexSelector
  , statusSelector
  , setStatusSelector
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

-- | Initialize an MTRDoorLockClusterGetWeekDayScheduleResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> responseValue -> error_ -> IO (Id MTRDoorLockClusterGetWeekDayScheduleResponseParams)
initWithResponseValue_error mtrDoorLockClusterGetWeekDayScheduleResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- weekDayIndex@
weekDayIndex :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
weekDayIndex mtrDoorLockClusterGetWeekDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "weekDayIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWeekDayIndex:@
setWeekDayIndex :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setWeekDayIndex mtrDoorLockClusterGetWeekDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "setWeekDayIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterGetWeekDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setUserIndex mtrDoorLockClusterGetWeekDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- status@
status :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
status mtrDoorLockClusterGetWeekDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setStatus mtrDoorLockClusterGetWeekDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- daysMask@
daysMask :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
daysMask mtrDoorLockClusterGetWeekDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "daysMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDaysMask:@
setDaysMask :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setDaysMask mtrDoorLockClusterGetWeekDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "setDaysMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startHour@
startHour :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
startHour mtrDoorLockClusterGetWeekDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "startHour") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartHour:@
setStartHour :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setStartHour mtrDoorLockClusterGetWeekDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "setStartHour:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startMinute@
startMinute :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
startMinute mtrDoorLockClusterGetWeekDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "startMinute") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartMinute:@
setStartMinute :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setStartMinute mtrDoorLockClusterGetWeekDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "setStartMinute:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endHour@
endHour :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
endHour mtrDoorLockClusterGetWeekDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "endHour") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndHour:@
setEndHour :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setEndHour mtrDoorLockClusterGetWeekDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "setEndHour:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endMinute@
endMinute :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
endMinute mtrDoorLockClusterGetWeekDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "endMinute") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndMinute:@
setEndMinute :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setEndMinute mtrDoorLockClusterGetWeekDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "setEndMinute:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterGetWeekDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterGetWeekDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetWeekDayScheduleResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

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

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

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

