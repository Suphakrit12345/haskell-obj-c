{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterGetYearDayScheduleParams@.
module ObjC.Matter.MTRDoorLockClusterGetYearDayScheduleParams
  ( MTRDoorLockClusterGetYearDayScheduleParams
  , IsMTRDoorLockClusterGetYearDayScheduleParams(..)
  , yearDayIndex
  , setYearDayIndex
  , userIndex
  , setUserIndex
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , yearDayIndexSelector
  , setYearDayIndexSelector
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

-- | @- yearDayIndex@
yearDayIndex :: IsMTRDoorLockClusterGetYearDayScheduleParams mtrDoorLockClusterGetYearDayScheduleParams => mtrDoorLockClusterGetYearDayScheduleParams -> IO (Id NSNumber)
yearDayIndex mtrDoorLockClusterGetYearDayScheduleParams  =
    sendMsg mtrDoorLockClusterGetYearDayScheduleParams (mkSelector "yearDayIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setYearDayIndex:@
setYearDayIndex :: (IsMTRDoorLockClusterGetYearDayScheduleParams mtrDoorLockClusterGetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleParams -> value -> IO ()
setYearDayIndex mtrDoorLockClusterGetYearDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetYearDayScheduleParams (mkSelector "setYearDayIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterGetYearDayScheduleParams mtrDoorLockClusterGetYearDayScheduleParams => mtrDoorLockClusterGetYearDayScheduleParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterGetYearDayScheduleParams  =
    sendMsg mtrDoorLockClusterGetYearDayScheduleParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterGetYearDayScheduleParams mtrDoorLockClusterGetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleParams -> value -> IO ()
setUserIndex mtrDoorLockClusterGetYearDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetYearDayScheduleParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterGetYearDayScheduleParams mtrDoorLockClusterGetYearDayScheduleParams => mtrDoorLockClusterGetYearDayScheduleParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterGetYearDayScheduleParams  =
    sendMsg mtrDoorLockClusterGetYearDayScheduleParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterGetYearDayScheduleParams mtrDoorLockClusterGetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterGetYearDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetYearDayScheduleParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterGetYearDayScheduleParams mtrDoorLockClusterGetYearDayScheduleParams => mtrDoorLockClusterGetYearDayScheduleParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterGetYearDayScheduleParams  =
    sendMsg mtrDoorLockClusterGetYearDayScheduleParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterGetYearDayScheduleParams mtrDoorLockClusterGetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterGetYearDayScheduleParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterGetYearDayScheduleParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

