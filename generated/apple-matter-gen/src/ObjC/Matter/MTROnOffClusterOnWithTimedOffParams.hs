{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROnOffClusterOnWithTimedOffParams@.
module ObjC.Matter.MTROnOffClusterOnWithTimedOffParams
  ( MTROnOffClusterOnWithTimedOffParams
  , IsMTROnOffClusterOnWithTimedOffParams(..)
  , onOffControl
  , setOnOffControl
  , onTime
  , setOnTime
  , offWaitTime
  , setOffWaitTime
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , onOffControlSelector
  , setOnOffControlSelector
  , onTimeSelector
  , setOnTimeSelector
  , offWaitTimeSelector
  , setOffWaitTimeSelector
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

-- | @- onOffControl@
onOffControl :: IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams => mtrOnOffClusterOnWithTimedOffParams -> IO (Id NSNumber)
onOffControl mtrOnOffClusterOnWithTimedOffParams  =
    sendMsg mtrOnOffClusterOnWithTimedOffParams (mkSelector "onOffControl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOnOffControl:@
setOnOffControl :: (IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams, IsNSNumber value) => mtrOnOffClusterOnWithTimedOffParams -> value -> IO ()
setOnOffControl mtrOnOffClusterOnWithTimedOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOnOffClusterOnWithTimedOffParams (mkSelector "setOnOffControl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- onTime@
onTime :: IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams => mtrOnOffClusterOnWithTimedOffParams -> IO (Id NSNumber)
onTime mtrOnOffClusterOnWithTimedOffParams  =
    sendMsg mtrOnOffClusterOnWithTimedOffParams (mkSelector "onTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOnTime:@
setOnTime :: (IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams, IsNSNumber value) => mtrOnOffClusterOnWithTimedOffParams -> value -> IO ()
setOnTime mtrOnOffClusterOnWithTimedOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOnOffClusterOnWithTimedOffParams (mkSelector "setOnTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- offWaitTime@
offWaitTime :: IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams => mtrOnOffClusterOnWithTimedOffParams -> IO (Id NSNumber)
offWaitTime mtrOnOffClusterOnWithTimedOffParams  =
    sendMsg mtrOnOffClusterOnWithTimedOffParams (mkSelector "offWaitTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOffWaitTime:@
setOffWaitTime :: (IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams, IsNSNumber value) => mtrOnOffClusterOnWithTimedOffParams -> value -> IO ()
setOffWaitTime mtrOnOffClusterOnWithTimedOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOnOffClusterOnWithTimedOffParams (mkSelector "setOffWaitTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams => mtrOnOffClusterOnWithTimedOffParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOnOffClusterOnWithTimedOffParams  =
    sendMsg mtrOnOffClusterOnWithTimedOffParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams, IsNSNumber value) => mtrOnOffClusterOnWithTimedOffParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOnOffClusterOnWithTimedOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOnOffClusterOnWithTimedOffParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams => mtrOnOffClusterOnWithTimedOffParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOnOffClusterOnWithTimedOffParams  =
    sendMsg mtrOnOffClusterOnWithTimedOffParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams, IsNSNumber value) => mtrOnOffClusterOnWithTimedOffParams -> value -> IO ()
setServerSideProcessingTimeout mtrOnOffClusterOnWithTimedOffParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOnOffClusterOnWithTimedOffParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @onOffControl@
onOffControlSelector :: Selector
onOffControlSelector = mkSelector "onOffControl"

-- | @Selector@ for @setOnOffControl:@
setOnOffControlSelector :: Selector
setOnOffControlSelector = mkSelector "setOnOffControl:"

-- | @Selector@ for @onTime@
onTimeSelector :: Selector
onTimeSelector = mkSelector "onTime"

-- | @Selector@ for @setOnTime:@
setOnTimeSelector :: Selector
setOnTimeSelector = mkSelector "setOnTime:"

-- | @Selector@ for @offWaitTime@
offWaitTimeSelector :: Selector
offWaitTimeSelector = mkSelector "offWaitTime"

-- | @Selector@ for @setOffWaitTime:@
setOffWaitTimeSelector :: Selector
setOffWaitTimeSelector = mkSelector "setOffWaitTime:"

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

