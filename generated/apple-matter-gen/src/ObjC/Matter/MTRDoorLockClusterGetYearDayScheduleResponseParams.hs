{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterGetYearDayScheduleResponseParams@.
module ObjC.Matter.MTRDoorLockClusterGetYearDayScheduleResponseParams
  ( MTRDoorLockClusterGetYearDayScheduleResponseParams
  , IsMTRDoorLockClusterGetYearDayScheduleResponseParams(..)
  , initWithResponseValue_error
  , yearDayIndex
  , setYearDayIndex
  , userIndex
  , setUserIndex
  , status
  , setStatus
  , localStartTime
  , setLocalStartTime
  , localEndTime
  , setLocalEndTime
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , yearDayIndexSelector
  , setYearDayIndexSelector
  , userIndexSelector
  , setUserIndexSelector
  , statusSelector
  , setStatusSelector
  , localStartTimeSelector
  , setLocalStartTimeSelector
  , localEndTimeSelector
  , setLocalEndTimeSelector
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

-- | Initialize an MTRDoorLockClusterGetYearDayScheduleResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> responseValue -> error_ -> IO (Id MTRDoorLockClusterGetYearDayScheduleResponseParams)
initWithResponseValue_error mtrDoorLockClusterGetYearDayScheduleResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- yearDayIndex@
yearDayIndex :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
yearDayIndex mtrDoorLockClusterGetYearDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "yearDayIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setYearDayIndex:@
setYearDayIndex :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setYearDayIndex mtrDoorLockClusterGetYearDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "setYearDayIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterGetYearDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setUserIndex mtrDoorLockClusterGetYearDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- status@
status :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
status mtrDoorLockClusterGetYearDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setStatus mtrDoorLockClusterGetYearDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localStartTime@
localStartTime :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
localStartTime mtrDoorLockClusterGetYearDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "localStartTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalStartTime:@
setLocalStartTime :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setLocalStartTime mtrDoorLockClusterGetYearDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "setLocalStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localEndTime@
localEndTime :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
localEndTime mtrDoorLockClusterGetYearDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "localEndTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalEndTime:@
setLocalEndTime :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setLocalEndTime mtrDoorLockClusterGetYearDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "setLocalEndTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterGetYearDayScheduleResponseParams  =
    sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterGetYearDayScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetYearDayScheduleResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

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

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

