{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterGetHolidayScheduleResponseParams@.
module ObjC.Matter.MTRDoorLockClusterGetHolidayScheduleResponseParams
  ( MTRDoorLockClusterGetHolidayScheduleResponseParams
  , IsMTRDoorLockClusterGetHolidayScheduleResponseParams(..)
  , initWithResponseValue_error
  , holidayIndex
  , setHolidayIndex
  , status
  , setStatus
  , localStartTime
  , setLocalStartTime
  , localEndTime
  , setLocalEndTime
  , operatingMode
  , setOperatingMode
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , holidayIndexSelector
  , setHolidayIndexSelector
  , statusSelector
  , setStatusSelector
  , localStartTimeSelector
  , setLocalStartTimeSelector
  , localEndTimeSelector
  , setLocalEndTimeSelector
  , operatingModeSelector
  , setOperatingModeSelector
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

-- | Initialize an MTRDoorLockClusterGetHolidayScheduleResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> responseValue -> error_ -> IO (Id MTRDoorLockClusterGetHolidayScheduleResponseParams)
initWithResponseValue_error mtrDoorLockClusterGetHolidayScheduleResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- holidayIndex@
holidayIndex :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
holidayIndex mtrDoorLockClusterGetHolidayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "holidayIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHolidayIndex:@
setHolidayIndex :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setHolidayIndex mtrDoorLockClusterGetHolidayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "setHolidayIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- status@
status :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
status mtrDoorLockClusterGetHolidayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setStatus mtrDoorLockClusterGetHolidayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localStartTime@
localStartTime :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
localStartTime mtrDoorLockClusterGetHolidayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "localStartTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalStartTime:@
setLocalStartTime :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setLocalStartTime mtrDoorLockClusterGetHolidayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "setLocalStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localEndTime@
localEndTime :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
localEndTime mtrDoorLockClusterGetHolidayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "localEndTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalEndTime:@
setLocalEndTime :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setLocalEndTime mtrDoorLockClusterGetHolidayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "setLocalEndTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- operatingMode@
operatingMode :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
operatingMode mtrDoorLockClusterGetHolidayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "operatingMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperatingMode:@
setOperatingMode :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setOperatingMode mtrDoorLockClusterGetHolidayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "setOperatingMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterGetHolidayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterGetHolidayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetHolidayScheduleResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @holidayIndex@
holidayIndexSelector :: Selector
holidayIndexSelector = mkSelector "holidayIndex"

-- | @Selector@ for @setHolidayIndex:@
setHolidayIndexSelector :: Selector
setHolidayIndexSelector = mkSelector "setHolidayIndex:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

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

-- | @Selector@ for @operatingMode@
operatingModeSelector :: Selector
operatingModeSelector = mkSelector "operatingMode"

-- | @Selector@ for @setOperatingMode:@
setOperatingModeSelector :: Selector
setOperatingModeSelector = mkSelector "setOperatingMode:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

