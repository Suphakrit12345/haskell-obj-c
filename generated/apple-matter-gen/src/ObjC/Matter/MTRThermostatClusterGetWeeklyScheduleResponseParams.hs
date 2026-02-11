{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterGetWeeklyScheduleResponseParams@.
module ObjC.Matter.MTRThermostatClusterGetWeeklyScheduleResponseParams
  ( MTRThermostatClusterGetWeeklyScheduleResponseParams
  , IsMTRThermostatClusterGetWeeklyScheduleResponseParams(..)
  , initWithResponseValue_error
  , numberOfTransitionsForSequence
  , setNumberOfTransitionsForSequence
  , dayOfWeekForSequence
  , setDayOfWeekForSequence
  , modeForSequence
  , setModeForSequence
  , transitions
  , setTransitions
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , numberOfTransitionsForSequenceSelector
  , setNumberOfTransitionsForSequenceSelector
  , dayOfWeekForSequenceSelector
  , setDayOfWeekForSequenceSelector
  , modeForSequenceSelector
  , setModeForSequenceSelector
  , transitionsSelector
  , setTransitionsSelector
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

-- | Initialize an MTRThermostatClusterGetWeeklyScheduleResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> responseValue -> error_ -> IO (Id MTRThermostatClusterGetWeeklyScheduleResponseParams)
initWithResponseValue_error mtrThermostatClusterGetWeeklyScheduleResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrThermostatClusterGetWeeklyScheduleResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- numberOfTransitionsForSequence@
numberOfTransitionsForSequence :: IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams => mtrThermostatClusterGetWeeklyScheduleResponseParams -> IO (Id NSNumber)
numberOfTransitionsForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams  =
    sendMsg mtrThermostatClusterGetWeeklyScheduleResponseParams (mkSelector "numberOfTransitionsForSequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumberOfTransitionsForSequence:@
setNumberOfTransitionsForSequence :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> value -> IO ()
setNumberOfTransitionsForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterGetWeeklyScheduleResponseParams (mkSelector "setNumberOfTransitionsForSequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dayOfWeekForSequence@
dayOfWeekForSequence :: IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams => mtrThermostatClusterGetWeeklyScheduleResponseParams -> IO (Id NSNumber)
dayOfWeekForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams  =
    sendMsg mtrThermostatClusterGetWeeklyScheduleResponseParams (mkSelector "dayOfWeekForSequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayOfWeekForSequence:@
setDayOfWeekForSequence :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> value -> IO ()
setDayOfWeekForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterGetWeeklyScheduleResponseParams (mkSelector "setDayOfWeekForSequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeForSequence@
modeForSequence :: IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams => mtrThermostatClusterGetWeeklyScheduleResponseParams -> IO (Id NSNumber)
modeForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams  =
    sendMsg mtrThermostatClusterGetWeeklyScheduleResponseParams (mkSelector "modeForSequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeForSequence:@
setModeForSequence :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> value -> IO ()
setModeForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterGetWeeklyScheduleResponseParams (mkSelector "setModeForSequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitions@
transitions :: IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams => mtrThermostatClusterGetWeeklyScheduleResponseParams -> IO (Id NSArray)
transitions mtrThermostatClusterGetWeeklyScheduleResponseParams  =
    sendMsg mtrThermostatClusterGetWeeklyScheduleResponseParams (mkSelector "transitions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitions:@
setTransitions :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSArray value) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> value -> IO ()
setTransitions mtrThermostatClusterGetWeeklyScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterGetWeeklyScheduleResponseParams (mkSelector "setTransitions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams => mtrThermostatClusterGetWeeklyScheduleResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterGetWeeklyScheduleResponseParams  =
    sendMsg mtrThermostatClusterGetWeeklyScheduleResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterGetWeeklyScheduleResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterGetWeeklyScheduleResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @numberOfTransitionsForSequence@
numberOfTransitionsForSequenceSelector :: Selector
numberOfTransitionsForSequenceSelector = mkSelector "numberOfTransitionsForSequence"

-- | @Selector@ for @setNumberOfTransitionsForSequence:@
setNumberOfTransitionsForSequenceSelector :: Selector
setNumberOfTransitionsForSequenceSelector = mkSelector "setNumberOfTransitionsForSequence:"

-- | @Selector@ for @dayOfWeekForSequence@
dayOfWeekForSequenceSelector :: Selector
dayOfWeekForSequenceSelector = mkSelector "dayOfWeekForSequence"

-- | @Selector@ for @setDayOfWeekForSequence:@
setDayOfWeekForSequenceSelector :: Selector
setDayOfWeekForSequenceSelector = mkSelector "setDayOfWeekForSequence:"

-- | @Selector@ for @modeForSequence@
modeForSequenceSelector :: Selector
modeForSequenceSelector = mkSelector "modeForSequence"

-- | @Selector@ for @setModeForSequence:@
setModeForSequenceSelector :: Selector
setModeForSequenceSelector = mkSelector "setModeForSequence:"

-- | @Selector@ for @transitions@
transitionsSelector :: Selector
transitionsSelector = mkSelector "transitions"

-- | @Selector@ for @setTransitions:@
setTransitionsSelector :: Selector
setTransitionsSelector = mkSelector "setTransitions:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

