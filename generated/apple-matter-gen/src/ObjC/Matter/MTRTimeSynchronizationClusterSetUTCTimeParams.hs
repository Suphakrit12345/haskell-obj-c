{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterSetUtcTimeParams@.
module ObjC.Matter.MTRTimeSynchronizationClusterSetUtcTimeParams
  ( MTRTimeSynchronizationClusterSetUtcTimeParams
  , IsMTRTimeSynchronizationClusterSetUtcTimeParams(..)
  , utcTime
  , setUtcTime
  , granularity
  , setGranularity
  , timeSource
  , setTimeSource
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , utcTimeSelector
  , setUtcTimeSelector
  , granularitySelector
  , setGranularitySelector
  , timeSourceSelector
  , setTimeSourceSelector
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

-- | @- utcTime@
utcTime :: IsMTRTimeSynchronizationClusterSetUtcTimeParams mtrTimeSynchronizationClusterSetUtcTimeParams => mtrTimeSynchronizationClusterSetUtcTimeParams -> IO (Id NSNumber)
utcTime mtrTimeSynchronizationClusterSetUtcTimeParams  =
    sendMsg mtrTimeSynchronizationClusterSetUtcTimeParams (mkSelector "utcTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUtcTime:@
setUtcTime :: (IsMTRTimeSynchronizationClusterSetUtcTimeParams mtrTimeSynchronizationClusterSetUtcTimeParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetUtcTimeParams -> value -> IO ()
setUtcTime mtrTimeSynchronizationClusterSetUtcTimeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterSetUtcTimeParams (mkSelector "setUtcTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- granularity@
granularity :: IsMTRTimeSynchronizationClusterSetUtcTimeParams mtrTimeSynchronizationClusterSetUtcTimeParams => mtrTimeSynchronizationClusterSetUtcTimeParams -> IO (Id NSNumber)
granularity mtrTimeSynchronizationClusterSetUtcTimeParams  =
    sendMsg mtrTimeSynchronizationClusterSetUtcTimeParams (mkSelector "granularity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGranularity:@
setGranularity :: (IsMTRTimeSynchronizationClusterSetUtcTimeParams mtrTimeSynchronizationClusterSetUtcTimeParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetUtcTimeParams -> value -> IO ()
setGranularity mtrTimeSynchronizationClusterSetUtcTimeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterSetUtcTimeParams (mkSelector "setGranularity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeSource@
timeSource :: IsMTRTimeSynchronizationClusterSetUtcTimeParams mtrTimeSynchronizationClusterSetUtcTimeParams => mtrTimeSynchronizationClusterSetUtcTimeParams -> IO (Id NSNumber)
timeSource mtrTimeSynchronizationClusterSetUtcTimeParams  =
    sendMsg mtrTimeSynchronizationClusterSetUtcTimeParams (mkSelector "timeSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeSource:@
setTimeSource :: (IsMTRTimeSynchronizationClusterSetUtcTimeParams mtrTimeSynchronizationClusterSetUtcTimeParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetUtcTimeParams -> value -> IO ()
setTimeSource mtrTimeSynchronizationClusterSetUtcTimeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterSetUtcTimeParams (mkSelector "setTimeSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTimeSynchronizationClusterSetUtcTimeParams mtrTimeSynchronizationClusterSetUtcTimeParams => mtrTimeSynchronizationClusterSetUtcTimeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTimeSynchronizationClusterSetUtcTimeParams  =
    sendMsg mtrTimeSynchronizationClusterSetUtcTimeParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTimeSynchronizationClusterSetUtcTimeParams mtrTimeSynchronizationClusterSetUtcTimeParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetUtcTimeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTimeSynchronizationClusterSetUtcTimeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterSetUtcTimeParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTimeSynchronizationClusterSetUtcTimeParams mtrTimeSynchronizationClusterSetUtcTimeParams => mtrTimeSynchronizationClusterSetUtcTimeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTimeSynchronizationClusterSetUtcTimeParams  =
    sendMsg mtrTimeSynchronizationClusterSetUtcTimeParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTimeSynchronizationClusterSetUtcTimeParams mtrTimeSynchronizationClusterSetUtcTimeParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetUtcTimeParams -> value -> IO ()
setServerSideProcessingTimeout mtrTimeSynchronizationClusterSetUtcTimeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterSetUtcTimeParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @utcTime@
utcTimeSelector :: Selector
utcTimeSelector = mkSelector "utcTime"

-- | @Selector@ for @setUtcTime:@
setUtcTimeSelector :: Selector
setUtcTimeSelector = mkSelector "setUtcTime:"

-- | @Selector@ for @granularity@
granularitySelector :: Selector
granularitySelector = mkSelector "granularity"

-- | @Selector@ for @setGranularity:@
setGranularitySelector :: Selector
setGranularitySelector = mkSelector "setGranularity:"

-- | @Selector@ for @timeSource@
timeSourceSelector :: Selector
timeSourceSelector = mkSelector "timeSource"

-- | @Selector@ for @setTimeSource:@
setTimeSourceSelector :: Selector
setTimeSourceSelector = mkSelector "setTimeSource:"

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

