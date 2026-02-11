{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterStartActionWithDurationParams@.
module ObjC.Matter.MTRActionsClusterStartActionWithDurationParams
  ( MTRActionsClusterStartActionWithDurationParams
  , IsMTRActionsClusterStartActionWithDurationParams(..)
  , actionID
  , setActionID
  , invokeID
  , setInvokeID
  , duration
  , setDuration
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , actionIDSelector
  , setActionIDSelector
  , invokeIDSelector
  , setInvokeIDSelector
  , durationSelector
  , setDurationSelector
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

-- | @- actionID@
actionID :: IsMTRActionsClusterStartActionWithDurationParams mtrActionsClusterStartActionWithDurationParams => mtrActionsClusterStartActionWithDurationParams -> IO (Id NSNumber)
actionID mtrActionsClusterStartActionWithDurationParams  =
    sendMsg mtrActionsClusterStartActionWithDurationParams (mkSelector "actionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterStartActionWithDurationParams mtrActionsClusterStartActionWithDurationParams, IsNSNumber value) => mtrActionsClusterStartActionWithDurationParams -> value -> IO ()
setActionID mtrActionsClusterStartActionWithDurationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterStartActionWithDurationParams (mkSelector "setActionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- invokeID@
invokeID :: IsMTRActionsClusterStartActionWithDurationParams mtrActionsClusterStartActionWithDurationParams => mtrActionsClusterStartActionWithDurationParams -> IO (Id NSNumber)
invokeID mtrActionsClusterStartActionWithDurationParams  =
    sendMsg mtrActionsClusterStartActionWithDurationParams (mkSelector "invokeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInvokeID:@
setInvokeID :: (IsMTRActionsClusterStartActionWithDurationParams mtrActionsClusterStartActionWithDurationParams, IsNSNumber value) => mtrActionsClusterStartActionWithDurationParams -> value -> IO ()
setInvokeID mtrActionsClusterStartActionWithDurationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterStartActionWithDurationParams (mkSelector "setInvokeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- duration@
duration :: IsMTRActionsClusterStartActionWithDurationParams mtrActionsClusterStartActionWithDurationParams => mtrActionsClusterStartActionWithDurationParams -> IO (Id NSNumber)
duration mtrActionsClusterStartActionWithDurationParams  =
    sendMsg mtrActionsClusterStartActionWithDurationParams (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsMTRActionsClusterStartActionWithDurationParams mtrActionsClusterStartActionWithDurationParams, IsNSNumber value) => mtrActionsClusterStartActionWithDurationParams -> value -> IO ()
setDuration mtrActionsClusterStartActionWithDurationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterStartActionWithDurationParams (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRActionsClusterStartActionWithDurationParams mtrActionsClusterStartActionWithDurationParams => mtrActionsClusterStartActionWithDurationParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrActionsClusterStartActionWithDurationParams  =
    sendMsg mtrActionsClusterStartActionWithDurationParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRActionsClusterStartActionWithDurationParams mtrActionsClusterStartActionWithDurationParams, IsNSNumber value) => mtrActionsClusterStartActionWithDurationParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrActionsClusterStartActionWithDurationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterStartActionWithDurationParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRActionsClusterStartActionWithDurationParams mtrActionsClusterStartActionWithDurationParams => mtrActionsClusterStartActionWithDurationParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrActionsClusterStartActionWithDurationParams  =
    sendMsg mtrActionsClusterStartActionWithDurationParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRActionsClusterStartActionWithDurationParams mtrActionsClusterStartActionWithDurationParams, IsNSNumber value) => mtrActionsClusterStartActionWithDurationParams -> value -> IO ()
setServerSideProcessingTimeout mtrActionsClusterStartActionWithDurationParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterStartActionWithDurationParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionID@
actionIDSelector :: Selector
actionIDSelector = mkSelector "actionID"

-- | @Selector@ for @setActionID:@
setActionIDSelector :: Selector
setActionIDSelector = mkSelector "setActionID:"

-- | @Selector@ for @invokeID@
invokeIDSelector :: Selector
invokeIDSelector = mkSelector "invokeID"

-- | @Selector@ for @setInvokeID:@
setInvokeIDSelector :: Selector
setInvokeIDSelector = mkSelector "setInvokeID:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

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

