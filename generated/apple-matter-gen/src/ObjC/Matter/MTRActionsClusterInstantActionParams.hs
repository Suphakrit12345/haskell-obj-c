{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterInstantActionParams@.
module ObjC.Matter.MTRActionsClusterInstantActionParams
  ( MTRActionsClusterInstantActionParams
  , IsMTRActionsClusterInstantActionParams(..)
  , actionID
  , setActionID
  , invokeID
  , setInvokeID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , actionIDSelector
  , setActionIDSelector
  , invokeIDSelector
  , setInvokeIDSelector
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
actionID :: IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams => mtrActionsClusterInstantActionParams -> IO (Id NSNumber)
actionID mtrActionsClusterInstantActionParams  =
    sendMsg mtrActionsClusterInstantActionParams (mkSelector "actionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams, IsNSNumber value) => mtrActionsClusterInstantActionParams -> value -> IO ()
setActionID mtrActionsClusterInstantActionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterInstantActionParams (mkSelector "setActionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- invokeID@
invokeID :: IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams => mtrActionsClusterInstantActionParams -> IO (Id NSNumber)
invokeID mtrActionsClusterInstantActionParams  =
    sendMsg mtrActionsClusterInstantActionParams (mkSelector "invokeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInvokeID:@
setInvokeID :: (IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams, IsNSNumber value) => mtrActionsClusterInstantActionParams -> value -> IO ()
setInvokeID mtrActionsClusterInstantActionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterInstantActionParams (mkSelector "setInvokeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams => mtrActionsClusterInstantActionParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrActionsClusterInstantActionParams  =
    sendMsg mtrActionsClusterInstantActionParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams, IsNSNumber value) => mtrActionsClusterInstantActionParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrActionsClusterInstantActionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterInstantActionParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams => mtrActionsClusterInstantActionParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrActionsClusterInstantActionParams  =
    sendMsg mtrActionsClusterInstantActionParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams, IsNSNumber value) => mtrActionsClusterInstantActionParams -> value -> IO ()
setServerSideProcessingTimeout mtrActionsClusterInstantActionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterInstantActionParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

