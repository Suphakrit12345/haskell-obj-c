{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterAddThermostatSuggestionParams@.
module ObjC.Matter.MTRThermostatClusterAddThermostatSuggestionParams
  ( MTRThermostatClusterAddThermostatSuggestionParams
  , IsMTRThermostatClusterAddThermostatSuggestionParams(..)
  , presetHandle
  , setPresetHandle
  , effectiveTime
  , setEffectiveTime
  , expirationInMinutes
  , setExpirationInMinutes
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , presetHandleSelector
  , setPresetHandleSelector
  , effectiveTimeSelector
  , setEffectiveTimeSelector
  , expirationInMinutesSelector
  , setExpirationInMinutesSelector
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

-- | @- presetHandle@
presetHandle :: IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams => mtrThermostatClusterAddThermostatSuggestionParams -> IO (Id NSData)
presetHandle mtrThermostatClusterAddThermostatSuggestionParams  =
    sendMsg mtrThermostatClusterAddThermostatSuggestionParams (mkSelector "presetHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPresetHandle:@
setPresetHandle :: (IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams, IsNSData value) => mtrThermostatClusterAddThermostatSuggestionParams -> value -> IO ()
setPresetHandle mtrThermostatClusterAddThermostatSuggestionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAddThermostatSuggestionParams (mkSelector "setPresetHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- effectiveTime@
effectiveTime :: IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams => mtrThermostatClusterAddThermostatSuggestionParams -> IO (Id NSNumber)
effectiveTime mtrThermostatClusterAddThermostatSuggestionParams  =
    sendMsg mtrThermostatClusterAddThermostatSuggestionParams (mkSelector "effectiveTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEffectiveTime:@
setEffectiveTime :: (IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams, IsNSNumber value) => mtrThermostatClusterAddThermostatSuggestionParams -> value -> IO ()
setEffectiveTime mtrThermostatClusterAddThermostatSuggestionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAddThermostatSuggestionParams (mkSelector "setEffectiveTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- expirationInMinutes@
expirationInMinutes :: IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams => mtrThermostatClusterAddThermostatSuggestionParams -> IO (Id NSNumber)
expirationInMinutes mtrThermostatClusterAddThermostatSuggestionParams  =
    sendMsg mtrThermostatClusterAddThermostatSuggestionParams (mkSelector "expirationInMinutes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExpirationInMinutes:@
setExpirationInMinutes :: (IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams, IsNSNumber value) => mtrThermostatClusterAddThermostatSuggestionParams -> value -> IO ()
setExpirationInMinutes mtrThermostatClusterAddThermostatSuggestionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAddThermostatSuggestionParams (mkSelector "setExpirationInMinutes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams => mtrThermostatClusterAddThermostatSuggestionParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterAddThermostatSuggestionParams  =
    sendMsg mtrThermostatClusterAddThermostatSuggestionParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams, IsNSNumber value) => mtrThermostatClusterAddThermostatSuggestionParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterAddThermostatSuggestionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAddThermostatSuggestionParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams => mtrThermostatClusterAddThermostatSuggestionParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThermostatClusterAddThermostatSuggestionParams  =
    sendMsg mtrThermostatClusterAddThermostatSuggestionParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams, IsNSNumber value) => mtrThermostatClusterAddThermostatSuggestionParams -> value -> IO ()
setServerSideProcessingTimeout mtrThermostatClusterAddThermostatSuggestionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAddThermostatSuggestionParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presetHandle@
presetHandleSelector :: Selector
presetHandleSelector = mkSelector "presetHandle"

-- | @Selector@ for @setPresetHandle:@
setPresetHandleSelector :: Selector
setPresetHandleSelector = mkSelector "setPresetHandle:"

-- | @Selector@ for @effectiveTime@
effectiveTimeSelector :: Selector
effectiveTimeSelector = mkSelector "effectiveTime"

-- | @Selector@ for @setEffectiveTime:@
setEffectiveTimeSelector :: Selector
setEffectiveTimeSelector = mkSelector "setEffectiveTime:"

-- | @Selector@ for @expirationInMinutes@
expirationInMinutesSelector :: Selector
expirationInMinutesSelector = mkSelector "expirationInMinutes"

-- | @Selector@ for @setExpirationInMinutes:@
setExpirationInMinutesSelector :: Selector
setExpirationInMinutesSelector = mkSelector "setExpirationInMinutes:"

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

