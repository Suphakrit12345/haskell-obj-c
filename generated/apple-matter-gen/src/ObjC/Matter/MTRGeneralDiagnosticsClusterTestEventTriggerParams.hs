{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterTestEventTriggerParams@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterTestEventTriggerParams
  ( MTRGeneralDiagnosticsClusterTestEventTriggerParams
  , IsMTRGeneralDiagnosticsClusterTestEventTriggerParams(..)
  , enableKey
  , setEnableKey
  , eventTrigger
  , setEventTrigger
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , enableKeySelector
  , setEnableKeySelector
  , eventTriggerSelector
  , setEventTriggerSelector
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

-- | @- enableKey@
enableKey :: IsMTRGeneralDiagnosticsClusterTestEventTriggerParams mtrGeneralDiagnosticsClusterTestEventTriggerParams => mtrGeneralDiagnosticsClusterTestEventTriggerParams -> IO (Id NSData)
enableKey mtrGeneralDiagnosticsClusterTestEventTriggerParams  =
    sendMsg mtrGeneralDiagnosticsClusterTestEventTriggerParams (mkSelector "enableKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnableKey:@
setEnableKey :: (IsMTRGeneralDiagnosticsClusterTestEventTriggerParams mtrGeneralDiagnosticsClusterTestEventTriggerParams, IsNSData value) => mtrGeneralDiagnosticsClusterTestEventTriggerParams -> value -> IO ()
setEnableKey mtrGeneralDiagnosticsClusterTestEventTriggerParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterTestEventTriggerParams (mkSelector "setEnableKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- eventTrigger@
eventTrigger :: IsMTRGeneralDiagnosticsClusterTestEventTriggerParams mtrGeneralDiagnosticsClusterTestEventTriggerParams => mtrGeneralDiagnosticsClusterTestEventTriggerParams -> IO (Id NSNumber)
eventTrigger mtrGeneralDiagnosticsClusterTestEventTriggerParams  =
    sendMsg mtrGeneralDiagnosticsClusterTestEventTriggerParams (mkSelector "eventTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEventTrigger:@
setEventTrigger :: (IsMTRGeneralDiagnosticsClusterTestEventTriggerParams mtrGeneralDiagnosticsClusterTestEventTriggerParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterTestEventTriggerParams -> value -> IO ()
setEventTrigger mtrGeneralDiagnosticsClusterTestEventTriggerParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterTestEventTriggerParams (mkSelector "setEventTrigger:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGeneralDiagnosticsClusterTestEventTriggerParams mtrGeneralDiagnosticsClusterTestEventTriggerParams => mtrGeneralDiagnosticsClusterTestEventTriggerParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGeneralDiagnosticsClusterTestEventTriggerParams  =
    sendMsg mtrGeneralDiagnosticsClusterTestEventTriggerParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGeneralDiagnosticsClusterTestEventTriggerParams mtrGeneralDiagnosticsClusterTestEventTriggerParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterTestEventTriggerParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGeneralDiagnosticsClusterTestEventTriggerParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterTestEventTriggerParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGeneralDiagnosticsClusterTestEventTriggerParams mtrGeneralDiagnosticsClusterTestEventTriggerParams => mtrGeneralDiagnosticsClusterTestEventTriggerParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGeneralDiagnosticsClusterTestEventTriggerParams  =
    sendMsg mtrGeneralDiagnosticsClusterTestEventTriggerParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGeneralDiagnosticsClusterTestEventTriggerParams mtrGeneralDiagnosticsClusterTestEventTriggerParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterTestEventTriggerParams -> value -> IO ()
setServerSideProcessingTimeout mtrGeneralDiagnosticsClusterTestEventTriggerParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterTestEventTriggerParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enableKey@
enableKeySelector :: Selector
enableKeySelector = mkSelector "enableKey"

-- | @Selector@ for @setEnableKey:@
setEnableKeySelector :: Selector
setEnableKeySelector = mkSelector "setEnableKey:"

-- | @Selector@ for @eventTrigger@
eventTriggerSelector :: Selector
eventTriggerSelector = mkSelector "eventTrigger"

-- | @Selector@ for @setEventTrigger:@
setEventTriggerSelector :: Selector
setEventTriggerSelector = mkSelector "setEventTrigger:"

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

