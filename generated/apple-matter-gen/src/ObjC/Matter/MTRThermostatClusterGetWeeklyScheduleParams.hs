{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterGetWeeklyScheduleParams@.
module ObjC.Matter.MTRThermostatClusterGetWeeklyScheduleParams
  ( MTRThermostatClusterGetWeeklyScheduleParams
  , IsMTRThermostatClusterGetWeeklyScheduleParams(..)
  , daysToReturn
  , setDaysToReturn
  , modeToReturn
  , setModeToReturn
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , daysToReturnSelector
  , setDaysToReturnSelector
  , modeToReturnSelector
  , setModeToReturnSelector
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

-- | @- daysToReturn@
daysToReturn :: IsMTRThermostatClusterGetWeeklyScheduleParams mtrThermostatClusterGetWeeklyScheduleParams => mtrThermostatClusterGetWeeklyScheduleParams -> IO (Id NSNumber)
daysToReturn mtrThermostatClusterGetWeeklyScheduleParams  =
    sendMsg mtrThermostatClusterGetWeeklyScheduleParams (mkSelector "daysToReturn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDaysToReturn:@
setDaysToReturn :: (IsMTRThermostatClusterGetWeeklyScheduleParams mtrThermostatClusterGetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleParams -> value -> IO ()
setDaysToReturn mtrThermostatClusterGetWeeklyScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterGetWeeklyScheduleParams (mkSelector "setDaysToReturn:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeToReturn@
modeToReturn :: IsMTRThermostatClusterGetWeeklyScheduleParams mtrThermostatClusterGetWeeklyScheduleParams => mtrThermostatClusterGetWeeklyScheduleParams -> IO (Id NSNumber)
modeToReturn mtrThermostatClusterGetWeeklyScheduleParams  =
    sendMsg mtrThermostatClusterGetWeeklyScheduleParams (mkSelector "modeToReturn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeToReturn:@
setModeToReturn :: (IsMTRThermostatClusterGetWeeklyScheduleParams mtrThermostatClusterGetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleParams -> value -> IO ()
setModeToReturn mtrThermostatClusterGetWeeklyScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterGetWeeklyScheduleParams (mkSelector "setModeToReturn:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterGetWeeklyScheduleParams mtrThermostatClusterGetWeeklyScheduleParams => mtrThermostatClusterGetWeeklyScheduleParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterGetWeeklyScheduleParams  =
    sendMsg mtrThermostatClusterGetWeeklyScheduleParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterGetWeeklyScheduleParams mtrThermostatClusterGetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterGetWeeklyScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterGetWeeklyScheduleParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThermostatClusterGetWeeklyScheduleParams mtrThermostatClusterGetWeeklyScheduleParams => mtrThermostatClusterGetWeeklyScheduleParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThermostatClusterGetWeeklyScheduleParams  =
    sendMsg mtrThermostatClusterGetWeeklyScheduleParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThermostatClusterGetWeeklyScheduleParams mtrThermostatClusterGetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleParams -> value -> IO ()
setServerSideProcessingTimeout mtrThermostatClusterGetWeeklyScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterGetWeeklyScheduleParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @daysToReturn@
daysToReturnSelector :: Selector
daysToReturnSelector = mkSelector "daysToReturn"

-- | @Selector@ for @setDaysToReturn:@
setDaysToReturnSelector :: Selector
setDaysToReturnSelector = mkSelector "setDaysToReturn:"

-- | @Selector@ for @modeToReturn@
modeToReturnSelector :: Selector
modeToReturnSelector = mkSelector "modeToReturn"

-- | @Selector@ for @setModeToReturn:@
setModeToReturnSelector :: Selector
setModeToReturnSelector = mkSelector "setModeToReturn:"

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

