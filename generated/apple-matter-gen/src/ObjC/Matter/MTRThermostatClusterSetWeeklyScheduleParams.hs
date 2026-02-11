{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterSetWeeklyScheduleParams@.
module ObjC.Matter.MTRThermostatClusterSetWeeklyScheduleParams
  ( MTRThermostatClusterSetWeeklyScheduleParams
  , IsMTRThermostatClusterSetWeeklyScheduleParams(..)
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
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
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

-- | @- numberOfTransitionsForSequence@
numberOfTransitionsForSequence :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSNumber)
numberOfTransitionsForSequence mtrThermostatClusterSetWeeklyScheduleParams  =
    sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "numberOfTransitionsForSequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumberOfTransitionsForSequence:@
setNumberOfTransitionsForSequence :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setNumberOfTransitionsForSequence mtrThermostatClusterSetWeeklyScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "setNumberOfTransitionsForSequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dayOfWeekForSequence@
dayOfWeekForSequence :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSNumber)
dayOfWeekForSequence mtrThermostatClusterSetWeeklyScheduleParams  =
    sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "dayOfWeekForSequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayOfWeekForSequence:@
setDayOfWeekForSequence :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setDayOfWeekForSequence mtrThermostatClusterSetWeeklyScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "setDayOfWeekForSequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeForSequence@
modeForSequence :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSNumber)
modeForSequence mtrThermostatClusterSetWeeklyScheduleParams  =
    sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "modeForSequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeForSequence:@
setModeForSequence :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setModeForSequence mtrThermostatClusterSetWeeklyScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "setModeForSequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitions@
transitions :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSArray)
transitions mtrThermostatClusterSetWeeklyScheduleParams  =
    sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "transitions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitions:@
setTransitions :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSArray value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setTransitions mtrThermostatClusterSetWeeklyScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "setTransitions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterSetWeeklyScheduleParams  =
    sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterSetWeeklyScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThermostatClusterSetWeeklyScheduleParams  =
    sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setServerSideProcessingTimeout mtrThermostatClusterSetWeeklyScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSetWeeklyScheduleParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

