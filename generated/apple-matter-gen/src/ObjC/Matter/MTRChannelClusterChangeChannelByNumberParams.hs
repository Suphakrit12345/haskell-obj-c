{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterChangeChannelByNumberParams@.
module ObjC.Matter.MTRChannelClusterChangeChannelByNumberParams
  ( MTRChannelClusterChangeChannelByNumberParams
  , IsMTRChannelClusterChangeChannelByNumberParams(..)
  , majorNumber
  , setMajorNumber
  , minorNumber
  , setMinorNumber
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , majorNumberSelector
  , setMajorNumberSelector
  , minorNumberSelector
  , setMinorNumberSelector
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

-- | @- majorNumber@
majorNumber :: IsMTRChannelClusterChangeChannelByNumberParams mtrChannelClusterChangeChannelByNumberParams => mtrChannelClusterChangeChannelByNumberParams -> IO (Id NSNumber)
majorNumber mtrChannelClusterChangeChannelByNumberParams  =
    sendMsg mtrChannelClusterChangeChannelByNumberParams (mkSelector "majorNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMajorNumber:@
setMajorNumber :: (IsMTRChannelClusterChangeChannelByNumberParams mtrChannelClusterChangeChannelByNumberParams, IsNSNumber value) => mtrChannelClusterChangeChannelByNumberParams -> value -> IO ()
setMajorNumber mtrChannelClusterChangeChannelByNumberParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChangeChannelByNumberParams (mkSelector "setMajorNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minorNumber@
minorNumber :: IsMTRChannelClusterChangeChannelByNumberParams mtrChannelClusterChangeChannelByNumberParams => mtrChannelClusterChangeChannelByNumberParams -> IO (Id NSNumber)
minorNumber mtrChannelClusterChangeChannelByNumberParams  =
    sendMsg mtrChannelClusterChangeChannelByNumberParams (mkSelector "minorNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinorNumber:@
setMinorNumber :: (IsMTRChannelClusterChangeChannelByNumberParams mtrChannelClusterChangeChannelByNumberParams, IsNSNumber value) => mtrChannelClusterChangeChannelByNumberParams -> value -> IO ()
setMinorNumber mtrChannelClusterChangeChannelByNumberParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChangeChannelByNumberParams (mkSelector "setMinorNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRChannelClusterChangeChannelByNumberParams mtrChannelClusterChangeChannelByNumberParams => mtrChannelClusterChangeChannelByNumberParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrChannelClusterChangeChannelByNumberParams  =
    sendMsg mtrChannelClusterChangeChannelByNumberParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRChannelClusterChangeChannelByNumberParams mtrChannelClusterChangeChannelByNumberParams, IsNSNumber value) => mtrChannelClusterChangeChannelByNumberParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrChannelClusterChangeChannelByNumberParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChangeChannelByNumberParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRChannelClusterChangeChannelByNumberParams mtrChannelClusterChangeChannelByNumberParams => mtrChannelClusterChangeChannelByNumberParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrChannelClusterChangeChannelByNumberParams  =
    sendMsg mtrChannelClusterChangeChannelByNumberParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRChannelClusterChangeChannelByNumberParams mtrChannelClusterChangeChannelByNumberParams, IsNSNumber value) => mtrChannelClusterChangeChannelByNumberParams -> value -> IO ()
setServerSideProcessingTimeout mtrChannelClusterChangeChannelByNumberParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChangeChannelByNumberParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @majorNumber@
majorNumberSelector :: Selector
majorNumberSelector = mkSelector "majorNumber"

-- | @Selector@ for @setMajorNumber:@
setMajorNumberSelector :: Selector
setMajorNumberSelector = mkSelector "setMajorNumber:"

-- | @Selector@ for @minorNumber@
minorNumberSelector :: Selector
minorNumberSelector = mkSelector "minorNumber"

-- | @Selector@ for @setMinorNumber:@
setMinorNumberSelector :: Selector
setMinorNumberSelector = mkSelector "setMinorNumber:"

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

